{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Main (main) where

import Control.Concurrent.MVar (MVar, newEmptyMVar, newMVar, putMVar, readMVar, takeMVar, tryPutMVar, tryTakeMVar)
import Control.Exception (finally)
import Control.Monad (forever, void)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (Value (..), eitherDecode, encode, object, (.=))
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Encoding qualified as TLE
import MCP.Server
import Network.Wai.Application.Static (defaultFileServerSettings, ssIndices, ssMaxAge, staticApp)
import Network.WebSockets qualified as WS
import System.Environment (getArgs)
import WaiAppStatic.Types (MaxAge (..), unsafeToPiece)

import Network.Wai qualified as Wai
import Network.Wai.Handler.Warp qualified as Warp
import Network.Wai.Handler.WebSockets qualified as WaiWS

-- MCP type family instances (no auth, no per-session state)
type instance MCPHandlerState = GameBridge
type instance MCPHandlerUser = ()

-- | Shared state for bridging MCP tool calls to the WebSocket-connected frontend.
data GameBridge = GameBridge
    { wsConn :: MVar (Maybe WS.Connection)
    -- ^ The current WebSocket connection to the browser frontend (Nothing if not connected).
    , wsResponse :: MVar Value
    -- ^ Channel for receiving a response from the frontend after sending a command.
    }

newGameBridge :: IO GameBridge
newGameBridge = GameBridge <$> newMVar Nothing <*> newEmptyMVar

main :: IO ()
main = do
    (dir, port) <- parseArgs
    bridge <- newGameBridge
    mcpApp <- mkMCPApp bridge
    let static = staticApp (defaultFileServerSettings dir){ssIndices = [unsafeToPiece "index.html"], ssMaxAge = NoMaxAge}
    let httpApp = noCache $ routeMCP mcpApp static
    -- Layer WebSocket handling on top of the HTTP app
    let wsApp = WaiWS.websocketsOr WS.defaultConnectionOptions (wsHandler bridge) httpApp
    putStrLn $ "Serving " <> dir <> " on http://localhost:" <> show port
    putStrLn $ "MCP endpoint at http://localhost:" <> show port <> "/mcp"
    putStrLn $ "WebSocket endpoint at ws://localhost:" <> show port <> "/ws"
    Warp.run port wsApp

parseArgs :: IO (FilePath, Int)
parseArgs =
    getArgs >>= \case
        [] -> pure ("dist", 8000)
        [dir] -> pure (dir, 8000)
        [dir, portStr] -> pure (dir, read portStr)
        _ -> fail "Usage: quadris-server [DIR] [PORT]"

-- | Route /mcp requests to the MCP app, everything else to static files.
routeMCP :: Wai.Application -> Wai.Application -> Wai.Application
routeMCP mcpApp staticApp' req respond =
    case Wai.pathInfo req of
        ("mcp" : _) -> mcpApp req respond
        _ -> staticApp' req respond

-- | No-cache headers on all responses.
noCache :: Wai.Middleware
noCache app req respond =
    app req $ respond . Wai.mapResponseHeaders (++ noCacheHeaders)
  where
    noCacheHeaders =
        [ ("Cache-Control", "no-store, no-cache, must-revalidate")
        , ("Pragma", "no-cache")
        , ("Expires", "0")
        ]

-- ---------------------------------------------------------------------------
-- WebSocket handler
-- ---------------------------------------------------------------------------

-- | Handle incoming WebSocket connections from the browser frontend.
-- Only one frontend connection is supported at a time.
wsHandler :: GameBridge -> WS.ServerApp
wsHandler bridge pending = do
    -- Only accept connections to /ws path
    let reqPath = WS.requestPath (WS.pendingRequest pending)
    if reqPath /= "/ws"
        then WS.rejectRequest pending "Expected /ws path"
        else do
            conn <- WS.acceptRequest pending
            putStrLn "Frontend WebSocket connected"
            -- Replace any existing connection
            void $ tryTakeMVar bridge.wsConn
            putMVar bridge.wsConn (Just conn)
            -- Keep the connection alive and forward responses
            WS.withPingThread conn 30 (pure ()) $
                wsLoop bridge conn
                    `finally` do
                        putStrLn "Frontend WebSocket disconnected"
                        void $ tryTakeMVar bridge.wsConn
                        putMVar bridge.wsConn Nothing


-- | Read messages from the frontend WebSocket and deliver them to the response channel.
wsLoop :: GameBridge -> WS.Connection -> IO ()
wsLoop bridge conn = forever do
    msg <- WS.receiveData conn :: IO TL.Text
    case eitherDecode (TLE.encodeUtf8 msg) of
        Left err ->
            putStrLn $ "Failed to parse frontend response: " <> err
        Right val -> do
            -- Deliver the response to any waiting MCP tool handler
            void $ tryPutMVar bridge.wsResponse val

-- | Send a command to the frontend and wait for its response.
sendCommandToFrontend :: GameBridge -> Value -> IO (Either T.Text Value)
sendCommandToFrontend bridge cmd = do
    mConn <- readMVar bridge.wsConn
    case mConn of
        Nothing -> pure $ Left "No frontend connected via WebSocket"
        Just conn -> do
            -- Drain any stale response
            void $ tryTakeMVar bridge.wsResponse
            -- Send the command
            WS.sendTextData conn (encode cmd)
            -- Wait for response (with timeout would be nice, but keep it simple)
            response <- takeMVar bridge.wsResponse
            pure $ Right response

-- ---------------------------------------------------------------------------
-- MCP
-- ---------------------------------------------------------------------------

mkMCPApp :: GameBridge -> IO Wai.Application
mkMCPApp bridge = do
    let initialState =
            initMCPServerState
                bridge -- handler state
                Nothing -- no init hook
                Nothing -- no finalize hook
                serverCapabilities
                (Implementation "georgefstris" "0.1.0" Nothing)
                (Just serverInstructions)
                (handlers bridge)
    stateVar <- newMVar initialState
    pure $ simpleHttpApp stateVar

serverInstructions :: T.Text
serverInstructions =
    """
    You are playing Tetris (Georgefstris). The game board is a 10-wide by 18-tall grid. \
    Columns are numbered 0-9 (left to right), rows 0-17 (top to bottom). \
    Pieces spawn at the top-centre (column 4, row 0). \
    \
    The 7 standard Tetris pieces are: O, I, S, Z, L, J, T. \
    Each piece can be rotated: NoRotation, Rotation90, Rotation180, Rotation270. \
    The 'pos' field in a placement command specifies where the piece's origin cell lands. \
    \
    Workflow: \
    1. Call 'get_board_state' to see the current pile, active piece, and preview queue. \
    2. Decide where to place the current piece. \
    3. Call 'place_piece' with the desired column (x), row (y), and rotation. \
       The piece will be dropped and locked in place if the position is valid. \
       If invalid (collision or out of bounds), the call fails and the piece remains. \
    4. Repeat. The game ends when a new piece cannot spawn without colliding. \
    \
    Tips: \
    - Clear lines by filling entire rows. \
    - The preview queue shows upcoming pieces - plan ahead. \
    - Row 0 is the top of the board. Higher row numbers are lower on screen. \
    - Piece shapes relative to origin (all rotations apply to these coords): \
      O: (0,0),(0,1),(1,0),(1,1)  I: (-1,0),(0,0),(1,0),(2,0) \
      S: (-1,1),(0,0),(0,1),(1,0)  Z: (-1,0),(0,0),(0,1),(1,1) \
      L: (-1,0),(-1,1),(0,0),(1,0)  J: (-1,0),(0,0),(1,0),(1,1) \
      T: (-1,0),(0,0),(0,1),(1,0)
    """

serverCapabilities :: ServerCapabilities
serverCapabilities =
    ServerCapabilities
        { logging = Nothing
        , prompts = Nothing
        , resources = Nothing
        , tools = Just ToolsCapability{listChanged = Nothing}
        , completions = Nothing
        , experimental = Nothing
        }

handlers :: GameBridge -> ProcessHandlers
handlers bridge = withToolHandlers (mcpTools bridge) defaultProcessHandlers

mcpTools :: GameBridge -> [ToolHandler]
mcpTools bridge = [getBoardStateTool bridge, placePieceTool bridge]

-- | Tool: get_board_state
-- Sends a GetBoardState command to the frontend and returns the response.
getBoardStateTool :: GameBridge -> ToolHandler
getBoardStateTool bridge =
    toolHandler
        "get_board_state"
        ( Just
            """
            Get the current Tetris board state. Returns: \
            - 'board': a 10x18 grid where each cell is null (empty) or a piece letter (O/I/S/Z/L/J/T). \
              Row 0 is the top, row 17 is the bottom. Column 0 is leftmost, column 9 is rightmost. \
            - 'current_piece': the piece you must place, with its type, current position, and rotation. \
            - 'preview': list of upcoming pieces (next in queue). \
            - 'game_over': whether the game has ended. \
            - 'line_count': total lines cleared so far.
            """
        )
        (InputSchema "object" Nothing Nothing)
        \_ -> do
            let cmd = object ["tag" .= ("GetBoardState" :: T.Text)]
            result <- liftIO $ sendCommandToFrontend bridge cmd
            case result of
                Left err -> pure $ ProcessSuccess $ toolTextError err
                Right val -> pure $ ProcessSuccess $ toolTextResult [jsonToText val]

-- | Tool: place_piece
-- Sends a PlacePiece command to the frontend and returns the updated board state.
placePieceTool :: GameBridge -> ToolHandler
placePieceTool bridge =
    toolHandler
        "place_piece"
        ( Just
            """
            Place the current Tetris piece at an exact position on the board. \
            The piece is placed at the given (x, y) coordinates with the given rotation, \
            then locked into the pile immediately (equivalent to a hard drop at that position). \
            \
            The placement must be valid: all cells of the rotated piece offset by (x, y) must \
            land on empty cells within the 10x18 grid (or above the top, which is allowed). \
            If the placement is invalid (collision with existing pieces or out of bounds), \
            the call fails and the piece remains unplaced. \
            \
            After a successful placement, completed lines are cleared and the next piece \
            from the preview queue becomes the current piece. \
            \
            Returns the updated board state on success, or an error message on failure.
            """
        )
        ( InputSchema
            "object"
            ( Just $
                Map.fromList
                    [ ("x", object ["type" .= ("integer" :: T.Text), "description" .= ("Column position (0-9, left to right)" :: T.Text)])
                    , ("y", object ["type" .= ("integer" :: T.Text), "description" .= ("Row position (0-17, top to bottom)" :: T.Text)])
                    , ("rotation", object ["type" .= ("string" :: T.Text), "enum" .= (["NoRotation", "Rotation90", "Rotation180", "Rotation270"] :: [T.Text]), "description" .= ("Rotation to apply to the piece" :: T.Text)])
                    ]
            )
            (Just ["x", "y", "rotation"])
        )
        \args -> do
            let mx = args >>= Map.lookup "x" >>= asInt
            let my = args >>= Map.lookup "y" >>= asInt
            let mrot = args >>= Map.lookup "rotation" >>= asText
            case (mx, my, mrot) of
                (Just px, Just py, Just rot) -> do
                    let cmd =
                            object
                                [ "tag" .= ("PlacePiece" :: T.Text)
                                , "x" .= px
                                , "y" .= py
                                , "rotation" .= rot
                                ]
                    result <- liftIO $ sendCommandToFrontend bridge cmd
                    case result of
                        Left err -> pure $ ProcessSuccess $ toolTextError err
                        Right val -> pure $ ProcessSuccess $ toolTextResult [jsonToText val]
                _ ->
                    pure $ ProcessSuccess $ toolTextError "Invalid arguments: 'x' (integer), 'y' (integer), and 'rotation' (string) are required"

-- | Render a JSON Value as Text for tool output.
jsonToText :: Value -> T.Text
jsonToText = TL.toStrict . TLE.decodeUtf8 . encode

-- | Extract an Int from a JSON Value.
asInt :: Value -> Maybe Int
asInt (Number n) = Just (round n)
asInt _ = Nothing

-- | Extract Text from a JSON Value.
asText :: Value -> Maybe T.Text
asText (String t) = Just t
asText _ = Nothing
