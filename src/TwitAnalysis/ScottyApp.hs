{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module TwitAnalysis.ScottyApp where

import Data.String (fromString)
import Network.Wai.Middleware.RequestLogger (logStdout)
import qualified Web.Scotty as Scotty
import qualified Web.Scotty.Session as Session

import TwitAnalysis.AppEnv (Env(..), newEnv)
import qualified TwitAnalysis.ButlerDemo as Butler
import qualified TwitAnalysis.LoginFlow as LoginFlow
import qualified TwitAnalysis.MiscWaiMiddleware as Middle
import qualified TwitAnalysis.OAuth.AuthFlow as Auth
import qualified TwitAnalysis.OAuth.Proxy as MyProxy
import qualified TwitAnalysis.TwitterApiCallDemo as ApiDemo

scottyApp :: Env -> Scotty.ScottyM ()
scottyApp Env { envAuthEnv
              , envButlerEnv
              , envOAuthCallbackPath
              , envLoginEnv
              , envHomePagePath
              , envLoginPath
              , envHttpMan
              , envApiCallDemoEnv
              } = do
  Scotty.get "/" (Scotty.redirect "/index.html") -- go to static dir
  Scotty.get
    (s envLoginPath)
    (LoginFlow.viewLogin envLoginEnv envAuthEnv envHomePagePath)
  Scotty.get
    (s envOAuthCallbackPath)
    (LoginFlow.handleOAuthCallback envLoginEnv envAuthEnv envHomePagePath)
  Scotty.get (s envHomePagePath) (LoginFlow.viewHome envLoginPath envLoginEnv)
  Butler.registerButlerRoutes envButlerEnv
  Scotty.get
    "/whoami"
    (ApiDemo.handleCurrentUser
       envApiCallDemoEnv
       envAuthEnv
       envLoginEnv
       envLoginPath)
  Scotty.get
    "/compose-tweet"
    (ApiDemo.showTweetPrompt envLoginEnv envLoginPath "/submit-tweet")
  Scotty.post
    "/submit-tweet"
    (ApiDemo.handleTweetSubmit
       envApiCallDemoEnv
       envAuthEnv
       envLoginEnv
       envLoginPath)
  MyProxy.registerPassthruEndpoint
    "/to-twitter"
    envHttpMan
    (Auth.envClientCred envAuthEnv)
    Auth.myOAuthServer
    envLoginEnv
  where
    s = fromString

-- | Start the server
startServer :: IO ()
startServer = do
  env@Env {envPort} <- newEnv
  Scotty.scotty envPort $ do
    Scotty.middleware logStdout
    Scotty.middleware Middle.justFavicon
    Scotty.middleware Middle.myStaticMiddleware
    scottyApp env
