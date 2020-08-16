module Main where

import Network.Wai.Middleware.RequestLogger (logStdout)
import qualified Web.Scotty as Scotty

import qualified Lib

main :: IO ()
main =
  Scotty.scotty 5000 $ do
    Scotty.middleware logStdout
    Lib.scottyDefinition
