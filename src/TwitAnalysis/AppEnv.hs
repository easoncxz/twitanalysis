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

import TwitAnalysis.Utils (mintercalate)

-- | A hierarchical-composition of smaller Env types from various modules!
data Env =
  Env
    { envPort :: Int
    , envLoginEnv :: Login.Env
    , envAuthEnv :: Auth.Env
    , envButlerEnv :: Butler.Env
    , envOAuthCallbackPath :: String
    , envHomePagePath :: String
    , envLoginPath :: String
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
  let baseUrl = "http://localhost:" ++ show envPort
  let envOAuthCallbackPath = "/oauth-callback"
      envHomePagePath = "/home"
      envLoginPath = "/login"
  envAuthEnv <-
    Auth.newEnv
      (Auth.BaseUrl baseUrl)
      (Auth.OAuthCallbackPath envOAuthCallbackPath)
      Nothing
  envButlerEnv <- Butler.newEnv
  envLoginEnv <- Login.newEnv
  return
    Env
      { envPort
      , envAuthEnv
      , envButlerEnv
      , envOAuthCallbackPath
      , envLoginEnv
      , envHomePagePath
      , envLoginPath
      }
