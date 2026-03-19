module Quadris where

import Control.Monad.Reader
import Control.Monad.State.Strict
import Data.Aeson qualified as Aeson
import Data.Bifunctor (second)
import Data.Colour.RGBSpace
import Data.Colour.RGBSpace.HSL
import Data.Either.Extra
import Data.Foldable
import Data.List.Extra
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Massiv.Array (Array)
import Data.Massiv.Array qualified as A
import Data.Maybe
import Data.Monoid.Extra
import Data.Time (NominalDiffTime)
import GHC.Generics (Generic, Generically (Generically))
import Linear (V2 (V2))
import Miso.Aeson
import Miso.CSS (Color)
import Miso.CSS qualified as MS
import Miso.JSON (FromJSON (..), ToJSON (..), object, withObject, (.:), (.=))
import Miso.String (MisoString, ToMisoString)
import System.Random.Stateful hiding (next, random)
import Util
import Util.Shuffle

data Opts = Opts
    { gridWidth :: Int
    , gridHeight :: Int
    , previewLength :: Word
    , ghost :: Bool
    , random :: IO StdGen
    , randomiser :: State StdGen (NonEmpty Piece)
    , topLevel :: Level
    , keyDelays :: KeyAction -> Maybe (NominalDiffTime, NominalDiffTime)
    , rate :: Level -> NominalDiffTime
    , colours :: Piece -> Color
    , keymap :: Int -> Maybe KeyAction
    }

opts :: Opts
opts =
    Opts
        { gridWidth = 10
        , gridHeight = 18
        , previewLength = 3
        , ghost = False
        , random = newStdGen
        , randomiser = flip shuffleM StateGenM . (:| enumerate) =<< uniformM StateGenM
        , topLevel
        , keyDelays = \case
            RotateLeft; RotateRight; HardDrop; Pause; Reset -> Nothing
            MoveLeft; MoveRight; SoftDrop; LevelDown; LevelUp -> Just (0.11, 0.022)
        , rate = \(fromIntegral . pred -> l) -> realToFrac @Double $ (0.8 - 0.007 * l) ** l
        , colours = \case
            O -> hsl' 0 62 54
            I -> hsl' 29 73 58
            S -> hsl' 203 72 75
            Z -> hsl' 147 34 49
            L -> hsl' 217 72 56
            J -> hsl' 275 37 60
            T -> hsl' 46 98 63
        , keymap = \case
            37 -> Just MoveLeft -- left arrow
            39 -> Just MoveRight -- right arrow
            90 -> Just RotateLeft -- z
            88 -> Just RotateRight -- x
            40 -> Just SoftDrop -- down arrow
            32 -> Just HardDrop -- space bar
            173 -> Just LevelDown -- minus
            61 -> Just LevelUp -- plus
            27 -> Just Pause -- esc
            82 -> Just Reset -- r
            _ -> Nothing
        }
  where
    topLevel = Level 20

-- TODO work out why Miso's `hsl` always just produces black on canvas
hsl' :: Double -> Double -> Double -> Color
hsl' h s l = uncurryRGB MS.rgb $ fmap (floor @Double . (* 255)) $ hsl h (s / 100) (l / 100)

newtype Level = Level Word
    deriving newtype (Eq, Ord, Show, Enum, Bounded, Num, Real, Integral, ToMisoString)

data KeyAction
    = MoveLeft
    | MoveRight
    | RotateLeft
    | RotateRight
    | SoftDrop
    | HardDrop
    | LevelDown
    | LevelUp
    | Pause
    | Reset
    deriving stock (Eq, Ord, Show, Enum, Bounded, Generic)
    deriving (Aeson.FromJSON, Aeson.ToJSON) via Generically KeyAction
    deriving (FromJSON, ToJSON) via (MisoAeson KeyAction)

data Piece = O | I | S | Z | L | J | T
    deriving stock (Eq, Ord, Show, Enum, Bounded, Generic)
    deriving (Aeson.FromJSON, Aeson.ToJSON) via Generically Piece
    deriving (FromJSON, ToJSON) via (MisoAeson Piece)
instance Uniform Piece where uniformM = uniformEnumM

-- inv: list has length 4, and its xs range from -1 to 2 (a single 2 for `I`), and ys from 0 to 1
shape :: Piece -> NonEmpty (V2 Int)
shape =
    (0 :|) . \case
        O -> [V2 0 1, V2 1 0, V2 1 1]
        I -> [V2 -1 0, V2 1 0, V2 2 0]
        S -> [V2 -1 1, V2 0 1, V2 1 0]
        Z -> [V2 -1 0, V2 0 1, V2 1 1]
        L -> [V2 -1 0, V2 -1 1, V2 1 0]
        J -> [V2 -1 0, V2 1 0, V2 1 1]
        T -> [V2 -1 0, V2 0 1, V2 1 0]

data ActivePiece = ActivePiece
    { piece :: Piece
    , pos :: V2 Int
    , rotation :: Rotation
    }
    deriving stock (Eq, Show, Generic)

newPiece :: Piece -> ActivePiece
newPiece piece = ActivePiece{piece, pos = V2 (opts.gridWidth `div` 2 - 1) 0, rotation = NoRotation}

-- TODO separate module? nothing like this in `linear` or even `diagrams`
-- actually, we could maybe just use `linear`'s matrices...
data Rotation
    = NoRotation
    | Rotation90
    | Rotation180
    | Rotation270
    deriving stock (Eq, Ord, Show, Enum, Bounded, Generic)
    deriving (Aeson.FromJSON, Aeson.ToJSON) via Generically Rotation
    deriving (FromJSON, ToJSON) via (MisoAeson Rotation)
rotate :: Rotation -> V2 Int -> V2 Int
rotate = flip \(V2 x y) -> \case
    NoRotation -> V2 x y
    Rotation90 -> V2 y -x
    Rotation180 -> V2 -x -y
    Rotation270 -> V2 -y x

-- inv: has width and height given by game opts, i.e.
-- (0,0) is the top-left
-- TODO use length-indexed vectors to statically avoid out-of-bounds errors?
-- TODO swap out `A.B` for more efficient representation? or at least use sparse array for empty cells
newtype Grid a = Grid (Array A.B A.Ix2 (Maybe a))
    deriving newtype (Eq, Show)
    deriving stock (Functor)
emptyGrid :: Grid a
emptyGrid = Grid $ A.replicate A.Seq (A.Sz2 opts.gridWidth opts.gridHeight) Nothing
lookupGrid :: Grid a -> V2 Int -> Either Bool (Maybe a) -- `Left True` means the location is above the top
lookupGrid (Grid g) (V2 x y) = if y < 0 then Left True else maybeToEither False $ g A.!? A.Ix2 x y
deconstructGrid :: (Monad m) => Grid a -> (V2 Int -> (Maybe a) -> m b) -> m ()
deconstructGrid (Grid g) = A.iforM_ g . \f (A.Ix2 x y) -> f (V2 x y)
addToGrid :: (Foldable t) => a -> t (V2 Int) -> Grid a -> Grid a
addToGrid a b (Grid g) =
    -- TODO can we avoid a copy?
    Grid $ A.withMArrayST_ g \gm -> for_ b \(V2 x y) ->
        A.modify_
            gm
            -- this assumes that the extra piece does not intersect occupied cells - if it does we overwrite
            (const $ pure $ Just a)
            (A.Ix2 x y)
intersectsGrid :: (Foldable t) => t (V2 Int) -> Grid a -> Bool
intersectsGrid vs (Grid g) =
    getAny $
        A.ifoldMono
            (\(A.Ix2 x y) e -> Any $ isJust e && any (V2 x y ==) vs)
            g
removeCompletedLines :: Grid a -> (Word, Grid a)
removeCompletedLines (Grid g) = second Grid $ fromMaybe (0, g) do
    -- TODO any failure would be a programmer error, so we just use `fromMaybe` to return the original
    -- that does imply there's probably a better way of doing this...
    g' <- foldrM (\i a -> A.compute <$> A.deleteColumnsM i 1 a) g completedRows
    g'' <- A.appendM 1 (A.replicate @A.B A.Seq (A.Sz2 opts.gridWidth $ length completedRows) Nothing) g'
    pure (genericLength completedRows, A.compute g'')
  where
    -- TODO it's a bit inefficient to `compute` on every iteration - there should be a better way
    -- TODO shouldn't it be `A.ifoldOuterSlice`, `A.deleteRowsM` etc. rather than columns?
    completedRows = A.ifoldInnerSlice (\i row -> mwhen (A.all isJust row) [i]) g

-- a monad for operations which produce finite lists of pieces
type RandomPieces = StateT [Piece] (State StdGen)
runRandomPieces :: ([Piece], StdGen) -> RandomPieces a -> (a, ([Piece], StdGen))
runRandomPieces (l, g) f = (\((a, b), c) -> (a, (b, c))) $ runStateGen g $ flip runStateT l . \StateGenM -> f
liftRandomiser :: State StdGen (NonEmpty Piece) -> RandomPieces Piece
liftRandomiser r = do
    (x, xs) <-
        gets uncons
            >>= maybe
                -- generate a new chunk of random pieces
                (neUncons <$> lift r)
                -- we already have pieces computed - take from those
                pure
    put xs
    pure x

-- ---------------------------------------------------------------------------
-- WebSocket command/response protocol
-- ---------------------------------------------------------------------------

-- | A command received from the server over WebSocket.
data Command
    = PlacePiece {x :: Int, y :: Int, rotation :: Rotation}
    | GetBoardState

instance FromJSON Command where
    parseJSON = withObject "Command" \o -> do
        tag <- o .: "tag"
        case (tag :: MisoString) of
            "PlacePiece" -> PlacePiece <$> o .: "x" <*> o .: "y" <*> o .: "rotation"
            "GetBoardState" -> pure GetBoardState
            _ -> fail "Unknown command tag"

instance ToJSON Command where
    toJSON GetBoardState = object ["tag" .= ("GetBoardState" :: MisoString)]
    toJSON (PlacePiece px py rot) = object ["tag" .= ("PlacePiece" :: MisoString), "x" .= px, "y" .= py, "rotation" .= rot]

-- | The response sent back to the server over WebSocket after a command.
data Response
    = BoardState
        { board :: [[MisoString]]
        , currentPiece :: PieceInfo
        , preview :: [MisoString]
        , gameOver :: Bool
        , lineCount :: Word
        }
    | PlaceError {reason :: MisoString}

instance ToJSON Response where
    toJSON (PlaceError r) = object ["tag" .= ("PlaceError" :: MisoString), "reason" .= r]
    toJSON BoardState{..} =
        object
            [ "tag" .= ("BoardState" :: MisoString)
            , "board" .= board
            , "currentPiece" .= currentPiece
            , "preview" .= preview
            , "gameOver" .= gameOver
            , "lineCount" .= lineCount
            ]

instance FromJSON Response where
    parseJSON = withObject "Response" \o -> do
        tag <- o .: "tag"
        case (tag :: MisoString) of
            "BoardState" -> BoardState <$> o .: "board" <*> o .: "currentPiece" <*> o .: "preview" <*> o .: "gameOver" <*> o .: "lineCount"
            "PlaceError" -> PlaceError <$> o .: "reason"
            _ -> fail "Unknown response tag"

-- | Info about the current active piece.
data PieceInfo = PieceInfo
    { piece :: MisoString
    , posX :: Int
    , posY :: Int
    , rotation :: MisoString
    }
    deriving stock (Generic)
instance FromJSON PieceInfo
instance ToJSON PieceInfo

-- | Serialize a piece to its string name.
pieceName :: Piece -> MisoString
pieceName = \case
    O -> "O"; I -> "I"; S -> "S"; Z -> "Z"; L -> "L"; J -> "J"; T -> "T"

-- | Serialize a rotation to its string name.
rotationName :: Rotation -> MisoString
rotationName = \case
    NoRotation -> "NoRotation"
    Rotation90 -> "Rotation90"
    Rotation180 -> "Rotation180"
    Rotation270 -> "Rotation270"

-- | Parse a rotation from its string name.
parseRotation :: MisoString -> Maybe Rotation
parseRotation = \case
    "NoRotation" -> Just NoRotation
    "Rotation90" -> Just Rotation90
    "Rotation180" -> Just Rotation180
    "Rotation270" -> Just Rotation270
    _ -> Nothing

-- | Convert a grid to a list of rows, where each cell is "" (empty) or a piece name string.
gridToBoard :: Grid Piece -> [[MisoString]]
gridToBoard (Grid g) =
    -- The grid is stored as (x, y) with x=column, y=row
    -- We need to produce rows: for each y, iterate over x
    [ [ case g A.!? A.Ix2 x y of
            Just (Just p) -> pieceName p
            _ -> ""
      | x <- [0 .. opts.gridWidth - 1]
      ]
    | y <- [0 .. opts.gridHeight - 1]
    ]
