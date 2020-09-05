{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module TwitAnalysis.ScottyApp where

import Network.Wai.Middleware.RequestLogger (logStdout)
import qualified Web.Scotty as Scotty
import qualified Web.Scotty.Session as Session

import TwitAnalysis.AppEnv
  ( AppEnv(AppEnv, appEnvAuthEnv, appEnvPort)
  , newAppEnv
  )
import qualified TwitAnalysis.ButlerDemo as Butler
import qualified TwitAnalysis.MiscWaiMiddleware as Middle
import qualified TwitAnalysis.OAuth.AuthFlow as AuthFlow

scottyApp ::
     Session.ScottySM Butler.MySessionType -> AppEnv -> Scotty.ScottyM ()
scottyApp sessionMan AppEnv {appEnvAuthEnv} = do
  Scotty.get "/" (Scotty.redirect "/index.html") -- go to static dir
  Scotty.get "/login" (AuthFlow.startOAuthFlow appEnvAuthEnv)
  Scotty.get
    AuthFlow.oauthCallbackPath
    (AuthFlow.handleOAuthCallback appEnvAuthEnv)
  Butler.registerButlerRoutes sessionMan

-- | Start the server
startServer :: IO ()
startServer = do
  sessionMan <- Session.createSessionManager
  appEnv@AppEnv {appEnvPort} <- newAppEnv
  Scotty.scotty appEnvPort $ do
    Scotty.middleware logStdout
    Scotty.middleware Middle.justFavicon
    Scotty.middleware Middle.myStaticMiddleware
    scottyApp sessionMan appEnv
