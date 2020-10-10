{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

module TwitAnalysis.AppEnv
  ( Env(..)
  , newEnv
  ) where

import Control.Concurrent.STM (TVar)
import qualified Control.Concurrent.STM as STM
import Control.Monad ((<=<))
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy.Char8 as BSL8
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import Data.String (IsString)
import Lens.Micro ((^.))
import qualified Network.HTTP.Client as HttpClient
import qualified Network.HTTP.Client.TLS as Tls
import qualified Network.OAuth.Types.Credentials as OAuthCred
import qualified System.Environment as Sys
import qualified Text.Read as Read

import qualified TwitAnalysis.ButlerDemo as Butler
import qualified TwitAnalysis.LoginFlow as Login
import qualified TwitAnalysis.OAuth.AuthFlow as Auth
import qualified TwitAnalysis.ReCaptcha as ReCaptcha
import qualified TwitAnalysis.TwitterApiCallDemo as ApiCallDemo
import TwitAnalysis.Utils (mintercalate)

-- | A hierarchical-composition of smaller Env types from various modules!
data Env =
  Env
    { envPort :: Int
    , envHttpMan :: HttpClient.Manager
    --
    , envOAuthCallbackPath :: String
    , envHomePagePath :: String
    , envLoginPath :: String
    --
    , envApiCallDemoEnv :: ApiCallDemo.Env
    , envLoginEnv :: Login.Env
    , envAuthEnv :: Auth.Env
    , envButlerEnv :: Butler.Env
    , envRecaptchaEnv :: ReCaptcha.Env
    }

newEnv :: IO Env
newEnv = do
  let defaultPort = 5000
  envPort <-
    do maybeStr <- Sys.lookupEnv "PORT"
       return . Maybe.fromMaybe defaultPort $ do
         str <- maybeStr
         num <- Read.readMaybe str
         return num
  envHttpMan <- HttpClient.newManager Tls.tlsManagerSettings
  let baseUrl = "http://localhost:" ++ show envPort
  let envOAuthCallbackPath = "/oauth-callback"
      envHomePagePath = "/app.html" -- connect through to frontend via static
      envLoginPath = "/login"
  envAuthEnv <-
    Auth.newEnv
      (Auth.BaseUrl baseUrl)
      (Auth.OAuthCallbackPath envOAuthCallbackPath)
      envHttpMan
  envButlerEnv <- Butler.newEnv
  envLoginEnv <- Login.newEnv
  envApiCallDemoEnv <- ApiCallDemo.newEnv envHttpMan
  envRecaptchaEnv <- ReCaptcha.newEnv envHttpMan
  return
    Env
      { envPort
      , envAuthEnv
      , envButlerEnv
      , envOAuthCallbackPath
      , envLoginEnv
      , envHomePagePath
      , envLoginPath
      , envHttpMan
      , envApiCallDemoEnv
      , envRecaptchaEnv
      }
