{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

-- Very dubious module: it wants very badly to form circular-dependencies with
-- every other module.
module TwitAnalysis.AppEnv
  ( AppEnv(..)
  , newAppEnv
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

import qualified TwitAnalysis.OAuth.AuthFlow as Auth

-- Resist importing any TwitAnalysis application modules!
import TwitAnalysis.Utils (mintercalate)

-- | A hierarchical-composition of smaller Env types from various modules!
data AppEnv =
  AppEnv
    { appEnvPort :: Int
    , appEnvAuthEnv :: Auth.Env
    }

-- | TODO: RUNTIME_ENV documentation
newAppEnv :: IO AppEnv
newAppEnv = do
  let defaultPort = 5000
  appEnvPort <-
    do maybeStr <- Sys.lookupEnv "PORT"
       return . Maybe.fromMaybe defaultPort $ do
         str <- maybeStr
         num <- Read.readMaybe str
         return num
  let baseUrl = "http://localhost:" ++ show appEnvPort
  appEnvAuthEnv <- Auth.newEnv baseUrl Nothing
  return AppEnv {appEnvPort, appEnvAuthEnv}
