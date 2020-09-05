{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}

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

-- Resist importing any TwitAnalysis application modules!
import TwitAnalysis.Utils (mintercalate)

type RequestTokenCache
   = TVar (Map.Map BS.ByteString (OAuthCred.Token OAuthCred.Temporary))

data AppEnv =
  AppEnv
    { appEnvHttpManager :: HttpClient.Manager
    , appEnvOAuthClientCred :: OAuthCred.Cred OAuthCred.Client
    , appEnvRequestTokenCache :: RequestTokenCache
    , appEnvSelfServerBaseUrl :: String
    , appEnvSelfServerPort :: Int
    }

-- | TODO: RUNTIME_ENV documentation
newClientCred :: IO (OAuthCred.Cred OAuthCred.Client)
newClientCred = do
  key <- Sys.getEnv "TWITTER_CONSUMER_KEY"
  secret <- Sys.getEnv "TWITTER_CONSUMER_SECRET"
  return $
    OAuthCred.clientCred (OAuthCred.Token (BS8.pack key) (BS8.pack secret))

newRequestTokenCache :: IO RequestTokenCache
newRequestTokenCache = STM.atomically (STM.newTVar Map.empty)

-- | TODO: RUNTIME_ENV documentation
newAppEnv :: IO AppEnv
newAppEnv = do
  tlsMan <- HttpClient.newManager Tls.tlsManagerSettings
  clientCred <- newClientCred
  cache <- newRequestTokenCache
  putStrLn ("Using client credentials: " ++ show clientCred)
  let defaultPort = 5000
  port <-
    do maybeStr <- Sys.lookupEnv "PORT"
       return . Maybe.fromMaybe defaultPort $ do
         str <- maybeStr
         num <- Read.readMaybe str
         return num
  let base = "http://localhost:" ++ show port
  putStrLn ("I understand that I am running at this URL: " ++ base)
  return (AppEnv tlsMan clientCred cache base port)
