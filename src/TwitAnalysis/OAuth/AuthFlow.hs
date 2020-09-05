{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

-- | OAuth 1.0a Consumer authorisation flow to Twitter
module TwitAnalysis.OAuth.AuthFlow
  ( Env -- opaque
  , newEnv
  , startOAuthFlow
  , handleOAuthCallback
  , oauthCallbackPath
  ) where

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
import qualified Data.Text.Lazy as TL
import Lens.Micro ((^.))
import qualified Network.HTTP.Client as HttpClient
import qualified Network.HTTP.Client.TLS as Tls
import qualified Network.HTTP.Types.Status as Http
import qualified Network.OAuth.ThreeLegged as OAuthThreeL
import Network.OAuth.Types.Credentials
  ( Client
  , Cred
  , Permanent
  , Temporary
  , Token(Token)
  )
import qualified Network.OAuth.Types.Credentials as OAuthCred
import qualified Network.OAuth.Types.Params as OAuthParams
import qualified System.Environment as Sys
import qualified Web.Scotty as Scotty

import TwitAnalysis.Utils (elseDo, mintercalate)

type RequestTokenCache = TVar (Map.Map BS.ByteString (Token Temporary))

-- | Modular programming!
--
-- A little Env that can be composed as part of a bigger Env somewhere else.
data Env =
  UnsafeEnv
    { envRequestTokenCache :: RequestTokenCache
    , envHttpManager :: HttpClient.Manager
    , envClientCred :: Cred Client
    , envBaseUrl :: String
    }

-- | TODO: RUNTIME_ENV documentation
newEnv :: String -> Maybe HttpClient.Manager -> IO Env
newEnv envBaseUrl defHttpMan = do
  envRequestTokenCache <- STM.atomically (STM.newTVar Map.empty)
  envHttpManager <-
    defHttpMan `elseDo` HttpClient.newManager Tls.tlsManagerSettings
  envClientCred <- newClientCred
  putStrLn ("Using client credentials: " ++ show envClientCred)
  return
    UnsafeEnv {envRequestTokenCache, envHttpManager, envClientCred, envBaseUrl}
  where
    newClientCred :: IO (OAuthCred.Cred OAuthCred.Client)
    newClientCred = do
      key <- Sys.getEnv "TWITTER_CONSUMER_KEY"
      secret <- Sys.getEnv "TWITTER_CONSUMER_SECRET"
      return $
        OAuthCred.clientCred (OAuthCred.Token (BS8.pack key) (BS8.pack secret))

-- | TODO: maybe add some expiry logic, like a delayed-deletion
rememberRequestToken :: RequestTokenCache -> Token Temporary -> IO ()
rememberRequestToken cache tok =
  STM.atomically . STM.modifyTVar' cache $ Map.insert (tok ^. OAuthCred.key) tok

relativeUrl :: (IsString s, Monoid s) => s -> s
relativeUrl = ("https://api.twitter.com/1.1/" <>)

oauthCallbackPath :: IsString s => s
oauthCallbackPath = "/oauth-callback"

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

-- | Lower-level action
startOAuthFlow' ::
     (HttpClient.Manager, Cred Client, RequestTokenCache, String)
  -> IO StartOAuthFlowResult
startOAuthFlow' (man, clientCred, cache, selfServerBaseUrl) = do
  let apparentlyNecessary =
        myOAuthServer {OAuthParams.parameterMethod = OAuthParams.QueryString}
  eitherRequestToken <-
    HttpClient.responseBody <$>
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

startOAuthFlow :: Env -> Scotty.ActionM ()
startOAuthFlow UnsafeEnv { envBaseUrl = base
                         , envRequestTokenCache = cache
                         , envClientCred = clientCred
                         , envHttpManager = httpMan
                         } =
  liftIO (startOAuthFlow' (httpMan, clientCred, cache, base)) >>= \case
    OAuthRequestTokenError bs -> Scotty.raw bs
    OAuthRedirectToAuthorisationPage url -> Scotty.redirect url

data ExchangeAccessTokenResult
  = AccessTokenDenied BSL.ByteString
  | AccessTokenAcquired (Token Permanent)

-- | Lower-level action
handleOAuthCallback' ::
     (HttpClient.Manager, Cred Client, RequestTokenCache, String)
  -> (OAuthCred.Key, OAuthThreeL.Verifier)
  -> IO ExchangeAccessTokenResult
handleOAuthCallback' (httpMan, clientCred, cache, selfServerBaseUrl) (reqTokenKey, verifierBs) = do
  reqToken :: Token Temporary <-
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
    case HttpClient.responseBody result of
      Left bs -> AccessTokenDenied bs
      Right token -> AccessTokenAcquired token

handleOAuthCallback :: Env -> Scotty.ActionM ()
handleOAuthCallback UnsafeEnv { envBaseUrl = base
                              , envRequestTokenCache = cache
                              , envClientCred = clientCred
                              , envHttpManager = httpMan
                              } = do
  reqTokenKey :: BS.ByteString <- Scotty.param "oauth_token"
  verifierBs :: BS.ByteString <- Scotty.param "oauth_verifier"
  result <-
    liftIO $
    handleOAuthCallback'
      (httpMan, clientCred, cache, base)
      (reqTokenKey, verifierBs)
  case result of
    AccessTokenDenied lbs -> do
      liftIO $ do
        putStrLn
          ("Failed to swap request token for access token. Request body printed below.")
        BSL8.putStrLn lbs
      let msg :: IsString s => s
          msg = "Failed to obtain access token."
      Scotty.status Http.status401 {Http.statusMessage = msg}
      Scotty.html msg
    AccessTokenAcquired token -> do
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
