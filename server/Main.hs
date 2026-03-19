{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Main (main) where

import Control.Concurrent.MVar (newMVar)
import Data.Aeson (Value (..), object, (.=))
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import MCP.Server
import Network.Wai.Application.Static (defaultFileServerSettings, ssIndices, ssMaxAge, staticApp)
import System.Environment (getArgs)
import WaiAppStatic.Types (MaxAge (..), unsafeToPiece)

import Network.Wai qualified as Wai
import Network.Wai.Handler.Warp qualified as Warp

-- MCP type family instances (no auth, no per-session state)
type instance MCPHandlerState = ()
type instance MCPHandlerUser = ()

main :: IO ()
main = do
    (dir, port) <- parseArgs
    mcpApp <- mkMCPApp
    let static = staticApp (defaultFileServerSettings dir){ssIndices = [unsafeToPiece "index.html"], ssMaxAge = NoMaxAge}
    putStrLn $ "Serving " <> dir <> " on http://localhost:" <> show port
    putStrLn $ "MCP endpoint at http://localhost:" <> show port <> "/mcp"
    Warp.run port $ noCache $ routeMCP mcpApp static

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
-- MCP
-- ---------------------------------------------------------------------------

mkMCPApp :: IO Wai.Application
mkMCPApp = do
    let initialState =
            initMCPServerState
                () -- handler state
                Nothing -- no init hook
                Nothing -- no finalize hook
                serverCapabilities
                (Implementation "georgefstris" "0.1.0" Nothing)
                Nothing -- no instructions
                handlers
    stateVar <- newMVar initialState
    pure $ simpleHttpApp stateVar

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

handlers :: ProcessHandlers
handlers = withToolHandlers mcpTools defaultProcessHandlers

mcpTools :: [ToolHandler]
mcpTools = [echoTool]

echoTool :: ToolHandler
echoTool =
    toolHandler
        "echo"
        (Just "Echoes the input message back")
        (InputSchema "object" (Just $ Map.fromList [("message", object ["type" .= ("string" :: T.Text)])]) (Just ["message"]))
        \args -> do
            let msg = args >>= Map.lookup "message" >>= \case
                    String t -> Just t
                    _ -> Nothing
            pure case msg of
                Just txt -> ProcessSuccess $ toolTextResult [txt]
                Nothing -> ProcessSuccess $ toolTextError "Missing or invalid 'message' argument"
