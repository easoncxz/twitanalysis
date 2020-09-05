{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}

module Lib where

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
import qualified Data.Text.Lazy as TL
import Lens.Micro ((^.))
import qualified Network.HTTP.Client as HttpClient
import qualified Network.HTTP.Client.TLS as Tls
import qualified Network.HTTP.Types.Status as Http
import qualified Network.OAuth.Types.Credentials as OAuthCred
import qualified System.Environment as Sys
import qualified Text.Read as Read
import qualified Web.Scotty as Scotty

import qualified TwitterOAuthConsumer as TwitterO
import Utils (mintercalate)

data AppEnv =
  AppEnv
    { appEnvHttpManager :: HttpClient.Manager
    , appEnvOAuthClientCred :: OAuthCred.Cred OAuthCred.Client
    , appEnvRequestTokenCache :: TwitterO.RequestTokenCache
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

-- | TODO: RUNTIME_ENV documentation
newAppEnv :: IO AppEnv
newAppEnv = do
  tlsMan <- HttpClient.newManager Tls.tlsManagerSettings
  clientCred <- newClientCred
  cache <- TwitterO.newRequestTokenCache
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

startOAuthFlow :: AppEnv -> Scotty.ActionM ()
startOAuthFlow AppEnv { appEnvSelfServerBaseUrl = base
                      , appEnvRequestTokenCache = cache
                      , appEnvOAuthClientCred = clientCred
                      , appEnvHttpManager = httpMan
                      } =
  liftIO (TwitterO.startOAuthFlow (httpMan, clientCred, cache, base)) >>= \case
    TwitterO.OAuthRequestTokenError bs -> Scotty.raw bs
    TwitterO.OAuthRedirectToAuthorisationPage url -> Scotty.redirect url

handleOAuthCallback :: AppEnv -> Scotty.ActionM ()
handleOAuthCallback AppEnv { appEnvSelfServerBaseUrl = base
                           , appEnvRequestTokenCache = cache
                           , appEnvOAuthClientCred = clientCred
                           , appEnvHttpManager = httpMan
                           } = do
  reqTokenKey :: BS.ByteString <- Scotty.param "oauth_token"
  verifierBs :: BS.ByteString <- Scotty.param "oauth_verifier"
  result <-
    liftIO $
    TwitterO.handleOAuthCallback
      (httpMan, clientCred, cache, base)
      (reqTokenKey, verifierBs)
  case result of
    TwitterO.AccessTokenDenied lbs -> do
      liftIO $ do
        putStrLn
          ("Failed to swap request token for access token. Request body printed below.")
        BSL8.putStrLn lbs
      let msg :: IsString s => s
          msg = "Failed to obtain access token."
      Scotty.status Http.status401 {Http.statusMessage = msg}
      Scotty.html msg
    TwitterO.AccessTokenAcquired token -> do
      let OAuthCred.Token accTokenKey _ = token
      liftIO
        (putStrLn ("Successfully received access token: " ++ show accTokenKey))
      Scotty.html $
        mintercalate
          "\n"
          [ "Here is your access token: "
          , "<pre>"
          , TL.pack (show token)
          , "</pre>"
          ]
