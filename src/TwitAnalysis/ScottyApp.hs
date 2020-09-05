{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module TwitAnalysis.ScottyApp where

import Data.String (fromString)
import Network.Wai.Middleware.RequestLogger (logStdout)
import qualified Web.Scotty as Scotty
import qualified Web.Scotty.Session as Session

import TwitAnalysis.AppEnv (Env(..), newEnv)
import qualified TwitAnalysis.ButlerDemo as Butler
import qualified TwitAnalysis.MiscWaiMiddleware as Middle
import qualified TwitAnalysis.OAuth.AuthFlow as AuthFlow

scottyApp :: Env -> Scotty.ScottyM ()
scottyApp Env {envAuthEnv, envButlerEnv, envOAuthCallbackPath} = do
  Scotty.get "/" (Scotty.redirect "/index.html") -- go to static dir
  Scotty.get "/login" (AuthFlow.startOAuthFlow envAuthEnv)
  Scotty.get
    (fromString envOAuthCallbackPath)
    (AuthFlow.handleOAuthCallback envAuthEnv)
  Butler.registerButlerRoutes envButlerEnv

-- | Start the server
startServer :: IO ()
startServer = do
  env@Env {envPort} <- newEnv
  Scotty.scotty envPort $ do
    Scotty.middleware logStdout
    Scotty.middleware Middle.justFavicon
    Scotty.middleware Middle.myStaticMiddleware
    scottyApp env
