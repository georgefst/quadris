module Quadris.Miso (app, Model (..), initialModel) where

import Control.Concurrent
import Control.Monad
import Control.Monad.Extra
import Control.Monad.State.Strict
import Data.Bifunctor (bimap, first)
import Data.Bitraversable
import Data.Bool
import Data.Coerce
import Data.Foldable
import Data.Foldable1 qualified as NE
import Data.Function
import Data.IORef
import Data.List.NonEmpty (NonEmpty)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe
import Data.Monoid.Extra
import Data.Optics.Operators ((+=))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time
import GHC.Generics (Generic)
import GHCJS.Prim (JSValueRef)
import JSDOM.CanvasRenderingContext2D qualified as CanvasCtx
import JSDOM.Document qualified as Doc
import JSDOM.HTMLCanvasElement qualified as Canvas
import JSDOM.Types (CanvasRenderingContext2D, HTMLCanvasElement)
import JSDOM.Types qualified as Dom
import Language.Javascript.JSaddle
import Linear (R1 (_x), R2 (_y), V2 (V2))
import Optics hiding (uncons)
import Optics.State.Operators
import Quadris
import Reflex.Dom hiding (Pause, Reset, current)
import Safe (predDef, succDef)
import System.Random.Stateful hiding (next, random)
import Util
import Util.FixedLengthQueue qualified as FLQ

data Model = Model
    { pile :: Grid Piece -- the cells fixed in place
    , current :: ActivePiece
    , next :: FLQ.Queue Piece
    , paused :: Bool
    , level :: Level
    , random :: ([Piece], StdGen)
    , gameOver :: Bool
    , lineCount :: Word
    }
    deriving stock (Eq, Show, Generic)
initialModel :: StdGen -> Level -> Model
initialModel random0 level =
    Model
        { pile = emptyGrid
        , level
        , gameOver = False
        , lineCount = 0
        , paused = False
        , ..
        }
  where
    ((current, next), random) = runRandomPieces ([], random0) do
        curry (bimap newPiece FLQ.fromList)
            <$> liftRandomiser opts.randomiser
            <*> replicateM (fromIntegral opts.previewLength) (liftRandomiser opts.randomiser)

data Action
    = Tick
    | KeyAction KeyAction

gridCanvasAttributes :: Int -> Int -> Map Text Text
gridCanvasAttributes w h =
    -- TODO ideally we'd just use `attr` in CSS, instead of needing the variable, but it's not widely supported
    Map.fromList [("width", T.show w), ("height", T.show h), ("style", T.show w <> "; --canvas-height: " <> T.show h)]

drawGridCanvas ::
    (MonadJSM m) =>
    CanvasRenderingContext2D ->
    Int ->
    Int ->
    ((Piece -> Bool -> V2 Int -> m ()) -> m ()) ->
    m ()
drawGridCanvas ctx w h f = do
    -- TODO keep some canvas state rather than always redrawing everything?
    CanvasCtx.clearRect ctx 0 0 (fromIntegral w) (fromIntegral h)
    f \p ghost (V2 x y) -> do
        CanvasCtx.setFillStyle ctx (toJSString $ if ghost then "lightgrey" else toJSString $ opts.colours p)
        CanvasCtx.fillRect ctx (fromIntegral x) (fromIntegral y) 1 1

handleAction :: Action -> Model -> Model
handleAction =
    execState . \case
        Tick -> unlessM (use #paused) do
            notOver <- not <$> use #gameOver
            when notOver do
                success <- tryMove (+ V2 0 1)
                when (not success) do
                    fixPiece
                    gameOver <- uncurry intersectsGrid . first pieceTiles <$> use (fanout #current #pile)
                    #gameOver .= gameOver
        KeyAction MoveLeft -> void $ tryMove (- V2 1 0)
        KeyAction MoveRight -> void $ tryMove (+ V2 1 0)
        KeyAction RotateLeft -> void $ tryRotate \case
            O -> id
            I; S; Z -> bool NoRotation Rotation90 . (== NoRotation)
            L; J; T -> succDef minBound
        KeyAction RotateRight -> void $ tryRotate \case
            O -> id
            I; S; Z -> bool NoRotation Rotation90 . (== NoRotation)
            L; J; T -> predDef maxBound
        KeyAction SoftDrop -> void $ tryMove (+ V2 0 1)
        KeyAction HardDrop -> whileM (tryMove (+ V2 0 1)) >> fixPiece
        KeyAction LevelDown -> #level %= max 1 . predDef 1
        KeyAction LevelUp -> #level %= min opts.topLevel . succDef opts.topLevel
        KeyAction Pause -> #paused %= not
        KeyAction Reset -> do
            m <- get
            put $ initialModel (snd m.random) m.level

grid :: (MonadWidget t m) => Dynamic t Model -> m ()
grid model = elAttr "div" ("id" =: "grid") do
    ctx <- getCanvasContext2D =<< canvasDynAttr_ (attrs <$> model)
    performEvent_ $ drawModel ctx <$> updated model
  where
    attrs m =
        gridCanvasAttributes opts.gridWidth opts.gridHeight
            <> Map.fromList (mwhen m.gameOver [("class", "game-over")] <> mwhen m.paused [("class", "paused")])

drawModel :: (MonadJSM m) => CanvasRenderingContext2D -> Model -> m ()
drawModel ctx Model{..} =
    drawGridCanvas ctx opts.gridWidth opts.gridHeight $
        deconstructGrid
            ( addToGrid (current.piece, False) (pieceTiles current)
                . applyWhen
                    opts.ghost
                    ( addToGrid (current.piece, True) $
                        pieceTiles (while (pieceFits pile) (#pos %~ (+ V2 0 1)) current)
                    )
                $ (,False) <$> pile
            )
            . flip
            . maybe (const $ pure ())
            . uncurry

sidebar :: (MonadWidget t m) => Dynamic t Model -> m ()
sidebar model = elAttr "div" ("id" =: "sidebar") do
    el "div" do
        void
            . dyn
            $ ((.next) <$> model) <&> \pieces ->
                for_ (FLQ.toList pieces) \piece -> elClass "div" "next" do
                    let
                        ps = shape piece
                        vMin = V2 (NE.minimum $ (^. lensVL _x) <$> ps) (NE.minimum $ (^. lensVL _y) <$> ps)
                        vmax = V2 (NE.maximum $ (^. lensVL _x) <$> ps) (NE.maximum $ (^. lensVL _y) <$> ps)
                        V2 w h = vmax - vMin + 1
                    ctx <- getCanvasContext2D =<< canvasAttr_ (gridCanvasAttributes w h)
                    drawGridCanvas ctx w h \f -> for_ ((- vMin) <$> ps) $ f piece False
        elClass "div" "level" do
            el "div" $ text "Level:"
            el "div" $ dynText $ T.show . (.level) <$> model
        elClass "div" "line-count" do
            el "div" $ text "Lines cleared:"
            el "div" $ dynText $ T.show . (.lineCount) <$> model

app :: (MonadWidget t m) => Model -> m ()
app m0 = do
    body <- Doc.getBodyUnchecked =<< askDocument
    keyActionEv <- keyEvents body
    rec -- game state with variable-rate tick
        -- let rateDyn = opts.rate . (.level) <$> modelDyn
        rateDyn <- holdUniqDyn $ opts.rate . (.level) <$> modelDyn
        tickEv <- switchHold never =<< dyn (tickLossyFromPostBuildTime <$> rateDyn)
        modelDyn <- foldDyn ($) m0 $ mergeWith (.) [handleAction Tick <$ tickEv, handleAction . KeyAction <$> keyActionEv]
    el "div" do
        grid modelDyn
        sidebar modelDyn

pieceTiles :: ActivePiece -> NonEmpty (V2 Int)
pieceTiles ActivePiece{..} = (+ pos) . rotate rotation <$> shape piece

fixPiece :: (MonadState Model m) => m ()
fixPiece = do
    Model{current, next} <- get
    #pile %= addToGrid current.piece (pieceTiles current)
    next' <- #random %%= flip runRandomPieces (liftRandomiser opts.randomiser)
    fanout #current #next .= first newPiece (FLQ.shift next' next)
    removed <- #pile %%= removeCompletedLines
    #lineCount += removed

tryMove :: (MonadState Model m) => (V2 Int -> V2 Int) -> m Bool
tryMove f = tryEdit . (#pos %~ f) =<< use #current

tryRotate :: (MonadState Model m) => (Piece -> Rotation -> Rotation) -> m Bool
tryRotate f = tryEdit . (\p -> p & #rotation %~ f p.piece) =<< use #current

tryEdit :: (MonadState Model m) => ActivePiece -> m Bool
tryEdit p = do
    b <- flip pieceFits p <$> use #pile
    when b $ #current .= p
    pure b

pieceFits :: Grid a -> ActivePiece -> Bool
pieceFits g p =
    either getAll (all isNothing)
        . (.unwrap)
        . traverse (Validation . first All . lookupGrid g . (+ p.pos) . rotate p.rotation)
        $ shape p.piece

-- TODO vibe-coded, to match the type of
-- \body -> fmapMaybe toAction <$> wrapDomEvent body (elementOnEventName Keydown) getKeyEvent

{- | Key repeat system. Tracks keydown/keyup, fires immediately on press,
then repeats with custom initial/repeat delays. Supports multiple simultaneous keys.
-}
keyEvents ::
    (MonadWidget t m) =>
    Dom.HTMLElement ->
    m (Event t KeyAction)
keyEvents body = do
    keydowns <- wrapDomEvent body (elementOnEventName Keydown) getKeyEvent
    keyups <- wrapDomEvent body (elementOnEventName Keyup) getKeyEvent
    let toAction = opts.keymap . fromIntegral
        downActions = fmapMaybe toAction keydowns
        upActions = fmapMaybe toAction keyups

    -- per-key cancellation refs: on keydown create a new ref, on keyup set it to False
    cancelRefs <- liftIO $ newIORef (mempty :: Map KeyAction (IORef Bool))

    -- on keyup, cancel any running repeat for that key
    performEvent_ $
        upActions <&> \k -> liftIO do
            refs <- readIORef cancelRefs
            for_ (Map.lookup k refs) \ref -> writeIORef ref False

    -- on keydown, fire immediately and start a repeat thread if applicable
    repeatEv <-
        performEventAsync $
            downActions <&> \k fire -> liftIO do
                -- cancel any existing repeat for this key
                refs <- readIORef cancelRefs
                for_ (Map.lookup k refs) \ref -> writeIORef ref False
                -- create a new cancellation ref
                alive <- newIORef True
                modifyIORef' cancelRefs (Map.insert k alive)
                -- fire immediately
                fire k
                -- start repeat loop if this key has repeat delays
                for_ (opts.keyDelays k) \(initialDelay, repeatDelay) ->
                    void $ forkIO do
                        sleep initialDelay
                        fix \loop_ -> do
                            stillAlive <- readIORef alive
                            when stillAlive do
                                fire k
                                sleep repeatDelay
                                loop_

    pure repeatEv
  where
    sleep :: NominalDiffTime -> IO ()
    sleep = threadDelay . round . (* 1_000_000) . nominalDiffTimeToSeconds

getCanvasContext2D :: (MonadJSM m) => HTMLCanvasElement -> m CanvasRenderingContext2D
getCanvasContext2D canvas =
    liftJSM $ Dom.unsafeCastTo pFromJSVal =<< toJSVal =<< Canvas.getContext canvas ("2d" :: JSString) ([] :: [Text])

type C t m =
    ( Coercible (RawElement (DomBuilderSpace m)) (IORef JSValueRef)
    , DomBuilder t m
    , PostBuild t m
    , MonadJSM m
    )
canvasDynAttr' :: (C t m) => Dynamic t (Map Text Text) -> m a -> m (HTMLCanvasElement, a)
canvasDynAttr' attrs x = bitraverse (Dom.unsafeCastTo pFromJSVal . _element_raw) pure =<< elDynAttr' "canvas" attrs x
canvasAttr' :: (C t m) => Map Text Text -> m a -> m (HTMLCanvasElement, a)
canvasAttr' attrs x = bitraverse (Dom.unsafeCastTo pFromJSVal . _element_raw) pure =<< elAttr' "canvas" attrs x
canvasDynAttr_ :: (C t m) => Dynamic t (Map Text Text) -> m HTMLCanvasElement
canvasDynAttr_ attrs = fst <$> canvasDynAttr' attrs (pure ())
canvasAttr_ :: (C t m) => Map Text Text -> m HTMLCanvasElement
canvasAttr_ attrs = fst <$> canvasAttr' attrs (pure ())
