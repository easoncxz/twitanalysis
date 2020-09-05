{-# LANGUAGE OverloadedStrings #-}

module TwitAnalysis.ScottyApp where

import Network.Wai.Middleware.RequestLogger (logStdout)
import qualified Web.Scotty as Scotty
import qualified Web.Scotty.Session as Session

import TwitAnalysis.AppEnv (AppEnv(AppEnv, appEnvSelfServerPort), newAppEnv)
import qualified TwitAnalysis.ButlerDemo as Butler
import qualified TwitAnalysis.MiscWaiMiddleware as Middle
import qualified TwitAnalysis.OAuth.AuthFlow as AuthFlow

scottyApp ::
     Session.ScottySM Butler.MySessionType -> AppEnv -> Scotty.ScottyM ()
scottyApp sessionMan appEnv = do
  Scotty.get "/" (Scotty.redirect "/index.html") -- go to static dir
  Scotty.get "/login" (AuthFlow.startOAuthFlow appEnv)
  Scotty.get AuthFlow.oauthCallbackPath (AuthFlow.handleOAuthCallback appEnv)
  Butler.registerButlerRoutes sessionMan

-- | Start the server
startServer :: IO ()
startServer = do
  sessionMan <- Session.createSessionManager
  appEnv@AppEnv {appEnvSelfServerPort = port} <- newAppEnv
  Scotty.scotty port $ do
    Scotty.middleware logStdout
    Scotty.middleware Middle.justFavicon
    Scotty.middleware Middle.myStaticMiddleware
    scottyApp sessionMan appEnv
