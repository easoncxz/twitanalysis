{-# LANGUAGE OverloadedStrings #-}

module Main where

import Network.Wai.Middleware.RequestLogger (logStdout)
import qualified Web.Scotty as Scotty

import qualified Lib

scottyApp :: Lib.AppEnv -> Scotty.ScottyM ()
scottyApp appEnv = do
  Scotty.get "/" Lib.viewHomepage
  Scotty.get "/login" (Lib.startOAuthFlow appEnv)
  Scotty.get Lib.oauthCallbackPath (Lib.handleOAuthCallback appEnv)

main :: IO ()
main = do
  appEnv <- Lib.newAppEnv
  putStrLn
    ("Using client credentials: " ++ show (Lib.appEnvOAuthClientCred appEnv))
  Scotty.scotty 5000 $ do
    Scotty.middleware logStdout
    scottyApp appEnv
