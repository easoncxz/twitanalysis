{-# LANGUAGE OverloadedStrings #-}

module Main where

import Network.Wai.Middleware.RequestLogger (logStdout)
import qualified Web.Scotty as Scotty

import qualified Lib
import qualified MiscWaiMiddleware as Middle
import qualified TwitterOAuthConsumer as TwitterO

scottyApp :: TwitterO.AppEnv -> Scotty.ScottyM ()
scottyApp appEnv = do
  Scotty.get "/login" (Lib.startOAuthFlow appEnv)
  Scotty.get TwitterO.oauthCallbackPath (Lib.handleOAuthCallback appEnv)

main :: IO ()
main = do
  appEnv <- TwitterO.newAppEnv
  putStrLn
    ("Using client credentials: " ++
     show (TwitterO.appEnvOAuthClientCred appEnv))
  Scotty.scotty 5000 $ do
    Scotty.middleware logStdout
    Scotty.middleware Middle.justFavicon
    Scotty.middleware Middle.myStaticMiddleware
    scottyApp appEnv
