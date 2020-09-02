{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module TwitterOAuthConsumer where

import Control.Concurrent.STM (TVar)
import qualified Control.Concurrent.STM as STM
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as BSL8
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import Data.Monoid (Monoid, (<>), mconcat)
import Data.String (IsString, fromString)
import qualified Data.Text as Text
import qualified Data.Text.Lazy as TextL
import Lens.Micro ((^.))
import qualified Network.HTTP.Client as Http
import qualified Network.HTTP.Client.TLS as Tls
import qualified Network.OAuth as OAuth
import qualified Network.OAuth.ThreeLegged as OAuth
import qualified Network.OAuth.Types.Credentials as OAuthCred
import qualified Network.OAuth.Types.Params as OAuthParams
import qualified System.Environment as Sys

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

-- | Copied and adapted from oauthenticated source
makeTemporaryTokenRequest ::
     (MonadIO m)
  => OAuthCred.Cred OAuth.Client
  -> OAuth.Server
  -> OAuth.ThreeLegged
  -> Http.Manager
  -> m Http.Request
makeTemporaryTokenRequest cr srv OAuth.ThreeLegged { OAuth.temporaryTokenRequest = temporaryTokenRequest
                                                   , OAuth.callback = callback
                                                   } man =
  liftIO $ do
    oax <- OAuth.freshOa cr
    let req =
          OAuth.sign
            (oax
               { OAuthParams.workflow =
                   OAuthParams.TemporaryTokenRequest callback
               })
            srv
            temporaryTokenRequest
    return req

-- | Copied and adapted from oauthenticated source
tryParseToken ::
     OAuth.Server -> BSL.ByteString -> Either BSL.ByteString (OAuth.Token ty)
tryParseToken srv lbs =
  case maybeParseToken lbs of
    Nothing -> Left lbs
    Just tok -> Right tok
  where
    maybeParseToken lbs = do
      (confirmed, tok) <- OAuth.fromUrlEncoded $ BSL.toStrict lbs
      case OAuthParams.oAuthVersion srv of
        OAuth.OAuthCommunity1 -> return tok
        _ ->
          if confirmed
            then return tok
            else fail "Must be confirmed"

mintercalate :: (Monoid s) => s -> [s] -> s
mintercalate sep list = mconcat (List.intersperse sep list)

relativeUrl :: (IsString s, Monoid s) => s -> s
relativeUrl = ("https://api.twitter.com/1.1/" <>)

oauthCallbackPath :: IsString s => s
oauthCallbackPath = "/oauth-callback"

-- | See Twitter docs here:
--
--    https://developer.twitter.com/en/docs/authentication/api-reference/request_token
--    https://developer.twitter.com/en/docs/authentication/api-reference/authorize
--    https://developer.twitter.com/en/docs/authentication/api-reference/access_token
myThreeLegged :: OAuth.ThreeLegged
myThreeLegged =
  Maybe.fromJust $
  OAuth.parseThreeLegged
    "https://api.twitter.com/oauth/request_token"
    "https://api.twitter.com/oauth/authorize"
    "https://api.twitter.com/oauth/access_token"
    (OAuth.Callback (fromString ("http://localhost:5000" <> oauthCallbackPath)))

myOAuthServer :: OAuthParams.Server
myOAuthServer =
  OAuthParams.Server
    OAuthParams.AuthorizationHeader
    OAuthParams.HmacSha1
    OAuthParams.OAuth1

data StartOAuthFlowResult
  = OAuthRequestTokenError BSL.ByteString
  | OAuthRedirectToAuthorisationPage TextL.Text

startOAuthFlow :: AppEnv -> IO StartOAuthFlowResult
startOAuthFlow (AppEnv man clientCred cache)
  --request <-
  --  makeTemporaryTokenRequest
  --    clientCred
  --    myOAuthServer {OAuthParams.parameterMethod = OAuthParams.QueryString}
  --    myThreeLegged
  --    man
  --liftIO $ putStr "Using the following request for request token: "
  --liftIO $ putStrLn (show request)
  --resp <- liftIO (Http.httpLbs request man)
  --let eitherRequestToken = tryParseToken myOAuthServer (Http.responseBody resp)
 = do
  eitherRequestToken <-
    let srv =
          myOAuthServer {OAuthParams.parameterMethod = OAuthParams.QueryString}
     in fmap Http.responseBody . liftIO $
        OAuth.requestTemporaryToken clientCred srv myThreeLegged man
  case eitherRequestToken of
    Left bs -> do
      liftIO $ putStrLn "Error: could not parse request token"
      return (OAuthRequestTokenError bs)
    Right reqToken -> do
      url <-
        liftIO $ do
          putStrLn ("Successfully received request token: " ++ show reqToken)
          STM.atomically $ do
            map <- STM.readTVar cache
            let map' = Map.insert (reqToken ^. OAuthCred.key) reqToken map
            STM.writeTVar cache map'
          let authorizeUrl =
                OAuth.buildAuthorizationUrl
                  (OAuthCred.temporaryCred reqToken clientCred)
                  myThreeLegged
          return (TextL.pack (show authorizeUrl))
      liftIO $ putStrLn ("Redirecting to URL: " ++ show url)
      return (OAuthRedirectToAuthorisationPage url)

handleOAuthCallback ::
     AppEnv
  -> BS.ByteString
  -> BS.ByteString
  -> IO (BS.ByteString, BS.ByteString)
handleOAuthCallback AppEnv { appEnvOAuthClientCred = clientCred
                           , appEnvHttpManager = httpMan
                           , appEnvRequestTokenCache = cache
                           } reqTokenKey verifierBs = do
  reqToken :: OAuthCred.Token OAuthCred.Temporary <-
    liftIO $ do
      maybeToken <-
        STM.atomically $ do
          map <- STM.readTVar cache
          return (Map.lookup reqTokenKey map)
      case maybeToken of
        Nothing -> fail ("Unrecognised request token: " ++ show reqTokenKey)
        Just token -> return token
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
    Right (OAuthCred.Token accTokenKey accTokenSecret) ->
      return (accTokenKey, accTokenSecret)
