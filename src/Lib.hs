{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Lib where

import Control.Concurrent.STM (TVar)
import qualified Control.Concurrent.STM as STM
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy.Char8 as BSL8
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import Data.Monoid (Monoid, (<>), mconcat)
import Data.String (IsString)
import qualified Data.Text as Text
import qualified Data.Text.Lazy as TextL
import Lens.Micro ((^.))
import qualified Network.HTTP.Client as Http
import qualified Network.HTTP.Client.TLS as Tls
import qualified Network.OAuth.ThreeLegged as OAuth
import qualified Network.OAuth.Types.Credentials as OAuthCred
import qualified Network.OAuth.Types.Params as OAuthParams
import qualified System.Environment as Sys
import qualified Text.Blaze as Blaze
import qualified Text.Blaze.Html.Renderer.Text as BlazeT
import qualified Web.Scotty as Scotty

mintercalate :: (Monoid s) => s -> [s] -> s
mintercalate sep list = mconcat (List.intersperse sep list)

relativeUrl :: (IsString s, Monoid s) => s -> s
relativeUrl = ("https://api.twitter.com/1.1/" <>)

-- | See Twitter docs here:
--
--    https://developer.twitter.com/en/docs/authentication/api-reference/request_token
--    https://developer.twitter.com/en/docs/authentication/api-reference/authorize
--    https://developer.twitter.com/en/docs/authentication/api-reference/access_token
myThreeLegged :: OAuth.ThreeLegged
myThreeLegged =
  Maybe.fromJust $
  OAuth.parseThreeLegged
    (relativeUrl "oauth/request_token")
    (relativeUrl "oauth/authorize")
    (relativeUrl "oauth/access_token")
    (OAuth.Callback "http://127.0.0.1/callback")

myOAuthServer :: OAuthParams.Server
myOAuthServer =
  OAuthParams.Server
    OAuthParams.QueryString
    OAuthParams.HmacSha1
    OAuthParams.OAuth1

newClientCred :: MonadIO m => m (OAuthCred.Cred OAuthCred.Client)
newClientCred =
  liftIO $ do
    key <- Sys.getEnv "TWITTER_CONSUMER_KEY"
    secret <- Sys.getEnv "TWITTER_CONSUMER_SECRET"
    return $
      OAuthCred.clientCred (OAuthCred.Token (BS8.pack key) (BS8.pack secret))

data AppEnv =
  AppEnv
    { appEnvHttpManager :: Http.Manager
    , appEnvOAuthClientCred :: OAuthCred.Cred OAuthCred.Client
    , appEnvRequestTokenCache :: TVar (Map.Map BS.ByteString (OAuthCred.Token OAuthCred.Temporary))
    }

newAppEnv :: IO AppEnv
newAppEnv = do
  tlsMan <- Http.newManager Tls.tlsManagerSettings
  clientCred <- newClientCred
  cache <- STM.atomically (STM.newTVar Map.empty)
  return (AppEnv tlsMan clientCred cache)

startOAuthFlow :: AppEnv -> Scotty.ActionM ()
startOAuthFlow (AppEnv man clientCred cache) = do
  eitherRequestToken <-
    liftIO
      (OAuth.requestTemporaryToken clientCred myOAuthServer myThreeLegged man)
  case Http.responseBody eitherRequestToken of
    Left bs -> fail (BSL8.unpack bs)
    Right reqToken -> do
      url <-
        liftIO $ do
          STM.atomically $ do
            map <- STM.readTVar cache
            let map' = Map.insert (reqToken ^. OAuthCred.key) reqToken map
            STM.writeTVar cache map'
          let authorizeUrl =
                OAuth.buildAuthorizationUrl
                  (OAuthCred.temporaryCred reqToken clientCred)
                  myThreeLegged
          putStrLn ("Redirecting to URL: " ++ show authorizeUrl)
          return (TextL.pack (show authorizeUrl))
      Scotty.redirect url

handleOAuthCallback :: AppEnv -> Scotty.ActionM ()
handleOAuthCallback AppEnv { appEnvOAuthClientCred = clientCred
                           , appEnvHttpManager = httpMan
                           , appEnvRequestTokenCache = cache
                           } = do
  reqTokenKey :: BS.ByteString <- Scotty.param "oauth_token"
  reqToken :: OAuthCred.Token OAuthCred.Temporary <-
    liftIO $ do
      maybeToken <-
        STM.atomically $ do
          map <- STM.readTVar cache
          return (Map.lookup reqTokenKey map)
      case maybeToken of
        Nothing -> fail ("Unrecognised request token: " ++ show reqTokenKey)
        Just token -> return token
  verifierBs :: BS.ByteString <- Scotty.param "oauth_verifier"
  response <-
    liftIO
      (OAuth.requestPermanentToken
         (OAuthCred.temporaryCred reqToken clientCred)
         myOAuthServer
         verifierBs
         myThreeLegged
         httpMan)
  case Http.responseBody response of
    Left bs -> fail (BSL8.unpack bs)
    Right accToken ->
      Scotty.html $
      mintercalate
        "\n"
        [ "Here is your access token: "
        , "<pre>"
        , BlazeT.renderHtml (Blaze.string (show accToken))
        , "</pre>"
        ]

viewHomepage :: Scotty.ActionM ()
viewHomepage = Scotty.html "<h1>Hello from Scotty!</h1>"
