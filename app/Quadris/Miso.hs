module Quadris.Miso (app, Model (..), initialModel) where

import Control.Monad
import Control.Monad.Extra
import Control.Monad.State.Strict
import Data.Bifunctor (bimap, first)
import Data.Bool
import Data.Foldable
import Data.Foldable1 qualified as NE
import Data.Function
import Data.Generics.Product
import Data.IntSet qualified as IntSet
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe
import Data.Monoid.Extra
import Data.Optics.Operators
import Data.Set qualified as Set
import Data.Word
import Foreign.Store
import GHC.Generics (Generic)
import Quadris
import Linear (R1 (_x), R2 (_y), V2 (V2))
import Miso hiding (Response, for, new, (--->), (<---), (<--->))
import Miso.CSS qualified as MS
import Miso.Canvas qualified as Canvas
import Miso.Html
import Miso.Html.Property hiding (for_)
import Miso.Optics
import Miso.WebSocket qualified as WS
import Optics hiding (uncons)
import Optics.State.Operators
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
    , websocket :: WS.WebSocket
    }
    deriving stock (Eq, Generic)
instance Show Model where
    show Model{..} = "Model{pile=" <> show pile <> ", current=" <> show current <> ", gameOver=" <> show gameOver <> ", lineCount=" <> show lineCount <> "}"
initialModel :: StdGen -> Level -> Model
initialModel random0 level =
    Model
        { pile = emptyGrid
        , level
        , gameOver = False
        , lineCount = 0
        , paused = False
        , websocket = WS.emptyWebSocket
        , ..
        }
  where
    ((current, next), random) = runRandomPieces ([], random0) do
        curry (bimap newPiece FLQ.fromList)
            <$> liftRandomiser opts.randomiser
            <*> replicateM (fromIntegral opts.previewLength) (liftRandomiser opts.randomiser)

data Action
    = NoOp (Maybe MisoString)
    | Init
    | KeyAction KeyAction
    -- WebSocket actions
    | WsConnect
    | WsOpen WS.WebSocket
    | WsClosed WS.Closed
    | WsCommand Command
    | WsError MisoString

gridCanvas ::
    Int ->
    Int ->
    [Attribute action] ->
    ((Piece -> Bool -> V2 Int -> Canvas.Canvas ()) -> Canvas.Canvas ()) ->
    View parent action
gridCanvas w h attrs f = Canvas.canvas
    ([width_ $ ms w, height_ $ ms h, cssVar "canvas-width" w, cssVar "canvas-height" h] <> attrs)
    (const $ pure ())
    \() -> do
        -- TODO keep some canvas state rather than always redrawing everything?
        Canvas.clearRect (0, 0, fromIntegral w, fromIntegral h)
        f \p ghost (V2 x y) -> do
            Canvas.fillStyle $ Canvas.ColorArg if ghost then MS.lightgrey else opts.colours p
            Canvas.fillRect (fromIntegral x, fromIntegral y, 1, 1)

-- | Build a 'Response' from the current model state.
mkBoardStateResponse :: Model -> Response
mkBoardStateResponse Model{..} =
    BoardState
        { board = gridToBoard pile
        , currentPiece =
            PieceInfo
                { piece = pieceName current.piece
                , posX = let V2 px _ = current.pos in px
                , posY = let V2 _ py = current.pos in py
                , rotation = rotationName current.rotation
                }
        , preview = pieceName <$> FLQ.toList next
        , gameOver
        , lineCount
        }

grid ::
    (HasType (FLQ.Queue Piece) parent, HasType Level parent, HasType Word parent) =>
    Word32 -> Model -> Component parent Model Action
grid foreignStoreId m0 =
    ( component
        m0
        ( (>> (io_ . writeStore (Store foreignStoreId) =<< get)) . \case
            NoOp s -> io_ $ traverse_ consoleLog s
            Init -> do
                subscribe keysPressedTopic KeyAction (const $ NoOp Nothing)
                -- Connect to the WebSocket server on the same host
                WS.connectJSON
                    (wsUrl :: WS.URL)
                    WsOpen
                    WsClosed
                    WsCommand
                    WsError
            WsConnect ->
                WS.connectJSON
                    (wsUrl :: WS.URL)
                    WsOpen
                    WsClosed
                    WsCommand
                    WsError
            WsOpen ws -> do
                #websocket .= ws
                io_ $ consoleLog "WebSocket connected"
                -- Send initial board state to the server
                m <- get
                WS.sendJSON ws (mkBoardStateResponse m)
            WsClosed _ -> do
                #websocket .= WS.emptyWebSocket
                io_ $ consoleLog "WebSocket closed"
            WsError err -> io_ $ consoleLog ("WebSocket error: " <> err)
            WsCommand cmd -> handleCommand cmd
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
        )
        ( \Model{..} ->
            gridCanvas
                opts.gridWidth
                opts.gridHeight
                ( mwhen gameOver [class_ "game-over"]
                    <> mwhen paused [class_ "paused"]
                )
                $ deconstructGrid
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
        )
    )
        { mount = Just Init
        , bindings =
            [ typed <--- #next
            , typed <--- #level
            , typed <--- #lineCount
            ]
        }
  where
    -- WebSocket URL: connect to ws://currenthost:8000/ws
    -- In production the server runs on port 8000
    wsUrl :: MisoString
    wsUrl = "ws://localhost:8000/ws"

    pieceTiles ActivePiece{..} = (+ pos) . rotate rotation <$> shape piece

    fixPiece = do
        Model{current, next} <- get
        #pile %= addToGrid current.piece (pieceTiles current)
        next' <- #random %%= flip runRandomPieces (liftRandomiser opts.randomiser)
        fanout #current #next .= first newPiece (FLQ.shift next' next)
        removed <- #pile %%= removeCompletedLines
        #lineCount += removed

    tryMove f = tryEdit . (#pos %~ f) =<< use #current
    tryRotate f = tryEdit . (\p -> p & #rotation %~ f p.piece) =<< use #current
    tryEdit p = do
        b <- flip pieceFits p <$> use #pile
        when b $ #current .= p
        pure b
    pieceFits g p =
        either getAll (all isNothing)
            . (.unwrap)
            . traverse (Validation . first All . lookupGrid g . (+ p.pos) . rotate p.rotation)
            $ shape p.piece

    -- Handle a command received over WebSocket.
    handleCommand GetBoardState = do
        m <- get
        ws <- use #websocket
        WS.sendJSON ws (mkBoardStateResponse m)
    handleCommand (PlacePiece px py rot) = do
        over <- use #gameOver
        if over
            then do
                ws <- use #websocket
                WS.sendJSON ws (PlaceError "Game is over")
            else do
                pile <- use #pile
                currentPiece <- use #current
                let targetPiece = currentPiece{pos = V2 px py, rotation = rot}
                if pieceFits pile targetPiece
                    then do
                        #current .= targetPiece
                        fixPiece
                        -- Check game over: does the new current piece collide?
                        newGameOver <- uncurry intersectsGrid . first pieceTiles <$> use (fanout #current #pile)
                        #gameOver .= newGameOver
                        -- Send updated board state
                        m <- get
                        ws <- use #websocket
                        WS.sendJSON ws (mkBoardStateResponse m)
                    else do
                        ws <- use #websocket
                        WS.sendJSON ws (PlaceError "Invalid placement: piece does not fit at the given position and rotation")

sidebar ::
    (HasType (FLQ.Queue Piece) parent, HasType Level parent, HasType Word parent) =>
    (FLQ.Queue Piece, Level, Word) -> Component parent (FLQ.Queue Piece, Level, Word) ()
sidebar m0 =
    ( component
        m0
        (\() -> pure ())
        ( \(pieces, level, lineCount) ->
            div_
                []
                $ ( FLQ.toList pieces <&> \piece ->
                        div_
                            [class_ "next"]
                            [ let
                                ps = shape piece
                                vMin = V2 (NE.minimum $ (^. lensVL _x) <$> ps) (NE.minimum $ (^. lensVL _y) <$> ps)
                                vmax = V2 (NE.maximum $ (^. lensVL _x) <$> ps) (NE.maximum $ (^. lensVL _y) <$> ps)
                                V2 w h = vmax - vMin + 1
                               in
                                gridCanvas w h [] \f -> for_ ((- vMin) <$> ps) $ f piece False
                            ]
                  )
                    <> [ div_
                            [class_ "level"]
                            [ div_ [] [text "Level:"]
                            , div_ [] [text $ ms level]
                            ]
                       , div_
                            [class_ "line-count"]
                            [ div_ [] [text "Lines cleared:"]
                            , div_ [] [text $ ms lineCount]
                            ]
                       ]
        )
    )
        { bindings =
            [ typed ---> _1
            , typed ---> _2
            , typed ---> _3
            ]
        }

-- TODO even though this component effectively has no view, it seems to be the best way to encapsulate our key stuff
-- we should aim to improve the API of `keyboardSub` and maybe even `Sub` itself in order to make this unnecessary
dummyKeyHandler :: Topic KeyAction -> Component parent (Map KeyAction Integer, Integer) (Either (KeyAction, Bool, Integer) [Int])
dummyKeyHandler keyTopic =
    ( component
        (mempty, 0)
        ( either
            ( \(k, new, i) -> do
                stillPressed <- if new then pure True else gets $ (== Just i) . Map.lookup k . fst
                when stillPressed do
                    io_ $ publish keyTopic k
                    for_ (opts.keyDelays k) \(initialKeyDelay, repeatKeyDelay) -> io $ do
                        liftIO $ threadDelay' if new then initialKeyDelay else repeatKeyDelay
                        pure $ Left (k, False, i)
            )
            \(Set.fromList . mapMaybe opts.keymap -> ks') -> do
                ks <- use _1
                freshlyPressed <-
                    Map.fromList <$> flip mapMaybeM (Set.toList ks') \k ->
                        if Map.member k ks
                            then pure Nothing
                            else Just . (k,) <$> (_2 <<%= succ)
                -- TODO the union is disjoint, and the keys of the result will be precisely the elements of `ks'`...
                -- we could probably somehow take advantage of this to simplify the code
                _1 .= Map.union freshlyPressed (Map.filterWithKey (const . (`elem` ks')) ks)
                void $ flip Map.traverseWithKey freshlyPressed $ flip \i -> issue . Left . (,True,i)
        )
        (\_ -> div_ [] [])
    )
        { subs = [keyboardSub $ Right . IntSet.toList]
        }

app :: Word32 -> Model -> Component parent (FLQ.Queue Piece, Level, Word) MisoString
app foreignStoreId m =
    component
        (m.next, m.level, m.lineCount)
        (io_ . consoleLog)
        ( \_ ->
            div_
                []
                [ div_
                    []
                    [ div_ [id_ "grid"] ["grid" +> grid foreignStoreId m]
                    , div_ [id_ "sidebar"] ["sidebar" +> sidebar (m.next, m.level, m.lineCount)]
                    ]
                , div_
                    [id_ "dummy-key-handler"]
                    ["dummy-key-handler" +> dummyKeyHandler keysPressedTopic]
                ]
        )

keysPressedTopic :: Topic KeyAction
keysPressedTopic = topic "keys-pressed"
