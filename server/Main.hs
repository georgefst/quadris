module Main (main) where

import Network.Wai.Application.Static (defaultFileServerSettings, ssIndices, ssMaxAge, staticApp)
import System.Environment (getArgs)
import WaiAppStatic.Types (MaxAge (..), unsafeToPiece)

import Network.Wai qualified as Wai
import Network.Wai.Handler.Warp qualified as Warp

main :: IO ()
main = do
    (dir, port) <- parseArgs
    putStrLn $ "Serving " <> dir <> " on http://localhost:" <> show port
    Warp.run port $ noCache $ staticApp (defaultFileServerSettings dir){ssIndices = [unsafeToPiece "index.html"], ssMaxAge = NoMaxAge}

parseArgs :: IO (FilePath, Int)
parseArgs =
    getArgs >>= \case
        [] -> pure ("dist", 8000)
        [dir] -> pure (dir, 8000)
        [dir, port] -> pure (dir, read port)
        _ -> fail "Usage: quadris-server [DIR] [PORT]"

noCache :: Wai.Middleware
noCache app req respond =
    app req $ respond . addHeaders
  where
    addHeaders resp = Wai.mapResponseHeaders (++ noCacheHeaders) resp
    noCacheHeaders =
        [ ("Cache-Control", "no-store, no-cache, must-revalidate")
        , ("Pragma", "no-cache")
        , ("Expires", "0")
        ]
