{-# LANGUAGE OverloadedStrings #-}

module Main where

import Network.Wai.Middleware.RequestLogger (logStdout)
import qualified Web.Scotty as Scotty
import qualified Web.Scotty.Session as Session

import qualified ButlerDemo as Butler
import qualified Lib
import qualified MiscWaiMiddleware as Middle
import qualified TwitterOAuthConsumer as TwitterO

scottyApp ::
     Session.ScottySM Butler.MySessionType -> Lib.AppEnv -> Scotty.ScottyM ()
scottyApp sessionMan appEnv = do
  Scotty.get "/" (Scotty.redirect "/index.html") -- go to static dir
  Scotty.get "/login" (Lib.startOAuthFlow appEnv)
  Scotty.get TwitterO.oauthCallbackPath (Lib.handleOAuthCallback appEnv)
  Butler.registerButlerRoutes sessionMan

main :: IO ()
main = do
  sessionMan <- Session.createSessionManager
  appEnv@Lib.AppEnv {Lib.appEnvSelfServerPort = port} <- Lib.newAppEnv
  Scotty.scotty port $ do
    Scotty.middleware logStdout
    Scotty.middleware Middle.justFavicon
    Scotty.middleware Middle.myStaticMiddleware
    scottyApp appEnv sessionMan
