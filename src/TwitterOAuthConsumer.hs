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
import qualified Network.OAuth.ThreeLegged as OAuthThreeL
import qualified Network.OAuth.Types.Credentials as OAuthCred
import qualified Network.OAuth.Types.Params as OAuthParams

import Utils (mintercalate)

type RequestTokenCache
   = TVar (Map.Map BS.ByteString (OAuthCred.Token OAuthCred.Temporary))

newRequestTokenCache :: IO RequestTokenCache
newRequestTokenCache = STM.atomically (STM.newTVar Map.empty)

-- | TODO: maybe add some expiry logic, like a delayed-deletion
rememberRequestToken ::
     RequestTokenCache -> OAuthCred.Token OAuthCred.Temporary -> IO ()
rememberRequestToken cache tok =
  STM.atomically . STM.modifyTVar' cache $ Map.insert (tok ^. OAuthCred.key) tok

relativeUrl :: (IsString s, Monoid s) => s -> s
relativeUrl = ("https://api.twitter.com/1.1/" <>)

oauthCallbackPath :: IsString s => s
oauthCallbackPath = "/oauth-callback"

selfServerBaseUrl :: IsString s => s
selfServerBaseUrl = "http://localhost:5000"

-- | See Twitter docs here:
--
--    https://developer.twitter.com/en/docs/authentication/api-reference/request_token
--    https://developer.twitter.com/en/docs/authentication/api-reference/authorize
--    https://developer.twitter.com/en/docs/authentication/api-reference/access_token
myThreeLegged :: String -> OAuthThreeL.ThreeLegged
myThreeLegged selfServerBaseUrl =
  Maybe.fromJust $
  OAuthThreeL.parseThreeLegged
    "https://api.twitter.com/oauth/request_token"
    "https://api.twitter.com/oauth/authorize"
    "https://api.twitter.com/oauth/access_token"
    (OAuthThreeL.Callback (fromString (selfServerBaseUrl <> oauthCallbackPath)))

myOAuthServer :: OAuthParams.Server
myOAuthServer =
  OAuthParams.Server
    OAuthParams.AuthorizationHeader
    OAuthParams.HmacSha1
    OAuthParams.OAuth1

data StartOAuthFlowResult
  = OAuthRequestTokenError BSL.ByteString
  | OAuthRedirectToAuthorisationPage TextL.Text

startOAuthFlow ::
     (Http.Manager, OAuthCred.Cred OAuthCred.Client, RequestTokenCache, String)
  -> IO StartOAuthFlowResult
startOAuthFlow (man, clientCred, cache, selfServerBaseUrl) = do
  let apparentlyNecessary =
        myOAuthServer {OAuthParams.parameterMethod = OAuthParams.QueryString}
  eitherRequestToken <-
    Http.responseBody <$>
    OAuthThreeL.requestTemporaryToken
      clientCred
      apparentlyNecessary
      (myThreeLegged selfServerBaseUrl)
      man
  case eitherRequestToken of
    Left bs -> do
      putStrLn "Error: could not parse request token"
      return (OAuthRequestTokenError bs)
    Right reqToken -> do
      putStrLn ("Successfully received request token: " ++ show reqToken)
      rememberRequestToken cache reqToken
      let authorizeUrl =
            OAuthThreeL.buildAuthorizationUrl
              (OAuthCred.temporaryCred reqToken clientCred)
              (myThreeLegged selfServerBaseUrl)
          url = TextL.pack (show authorizeUrl)
      putStrLn ("Redirecting to URL: " ++ show url)
      return (OAuthRedirectToAuthorisationPage url)

data ExchangeAccessTokenResult
  = AccessTokenDenied BSL.ByteString
  | AccessTokenAcquired (OAuthCred.Token OAuthCred.Permanent)

handleOAuthCallback ::
     (Http.Manager, OAuthCred.Cred OAuthCred.Client, RequestTokenCache, String)
  -> (OAuthCred.Key, OAuthThreeL.Verifier)
  -> IO ExchangeAccessTokenResult
handleOAuthCallback (httpMan, clientCred, cache, selfServerBaseUrl) (reqTokenKey, verifierBs) = do
  reqToken :: OAuthCred.Token OAuthCred.Temporary <-
    liftIO $ do
      maybeToken <-
        STM.atomically $ do
          map <- STM.readTVar cache
          return (Map.lookup reqTokenKey map)
      case maybeToken of
        Nothing -> fail ("Unrecognised request token: " ++ show reqTokenKey)
        Just token -> return token
  result <-
    liftIO
      (OAuthThreeL.requestPermanentToken
         (OAuthCred.temporaryCred reqToken clientCred)
         myOAuthServer
         verifierBs
         (myThreeLegged selfServerBaseUrl)
         httpMan)
  return $
    case Http.responseBody result of
      Left bs -> AccessTokenDenied bs
      Right token -> AccessTokenAcquired token
