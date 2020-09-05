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
     Lib.AppEnv -> Session.ScottySM Butler.MySessionType -> Scotty.ScottyM ()
scottyApp appEnv sessionMan = do
  Scotty.get "/" (Scotty.redirect "/index.html") -- go to static dir
  Scotty.get "/login" (Lib.startOAuthFlow appEnv)
  Scotty.get TwitterO.oauthCallbackPath (Lib.handleOAuthCallback appEnv)
  Scotty.get Butler.butlerGetPath (Butler.recogniseGuest sessionMan)
  Scotty.post Butler.butlerPostPath (Butler.registerGuest sessionMan)
  Scotty.post Butler.butlerPostPathLogout (Butler.forgetGuest sessionMan)

main :: IO ()
main = do
  appEnv@Lib.AppEnv {Lib.appEnvSelfServerPort = port} <- Lib.newAppEnv
  sessionMan <- Session.createSessionManager
  Scotty.scotty port $ do
    Scotty.middleware logStdout
    Scotty.middleware Middle.justFavicon
    Scotty.middleware Middle.myStaticMiddleware
    scottyApp appEnv sessionMan
