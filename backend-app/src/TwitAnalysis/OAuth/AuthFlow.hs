{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

-- | OAuth 1.0a Consumer authorisation flow to Twitter
module TwitAnalysis.OAuth.AuthFlow
  ( Env -- opaque
  , BaseUrl(..)
  , OAuthCallbackPath(..)
  , StartOAuthFlowResult(..)
  , ExchangeAccessTokenResult(..)
  , newEnv
  , startOAuthFlow'
  , handleOAuthCallback'
  , forfeitRequestToken
  , myOAuthServer
  , envClientCred
  ) where

import Control.Concurrent.STM (TVar)
import qualified Control.Concurrent.STM as STM
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as BSL
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import Data.Monoid (Monoid, (<>), mconcat)
import Data.String (IsString, fromString)
import qualified Data.Text.Lazy as TL
import Lens.Micro ((^.))
import qualified Network.HTTP.Client as HttpClient
import qualified Network.HTTP.Client.TLS as Tls
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
    , envCallbackPath :: String
    , envThreeLegged :: OAuthThreeL.ThreeLegged
    }

newtype BaseUrl =
  BaseUrl String

newtype OAuthCallbackPath =
  OAuthCallbackPath String

newEnv :: BaseUrl -> OAuthCallbackPath -> HttpClient.Manager -> IO Env
newEnv (BaseUrl envBaseUrl) (OAuthCallbackPath envCallbackPath) envHttpManager = do
  envRequestTokenCache <- STM.atomically (STM.newTVar Map.empty)
  envClientCred <- newClientCred
  putStrLn ("Using client credentials: " ++ show envClientCred)
  return
    UnsafeEnv
      { envRequestTokenCache
      , envHttpManager
      , envClientCred
      , envBaseUrl
      , envThreeLegged
      , envCallbackPath
      }
  where
    newClientCred :: IO (OAuthCred.Cred OAuthCred.Client)
    newClientCred = do
      key <- Sys.getEnv "TWITTER_CONSUMER_KEY"
      secret <- Sys.getEnv "TWITTER_CONSUMER_SECRET"
      return $
        OAuthCred.clientCred (OAuthCred.Token (BS8.pack key) (BS8.pack secret))
    -- | See Twitter docs here:
    --
    --    https://developer.twitter.com/en/docs/authentication/api-reference/request_token
    --    https://developer.twitter.com/en/docs/authentication/api-reference/authorize
    --    https://developer.twitter.com/en/docs/authentication/api-reference/access_token
    envThreeLegged :: OAuthThreeL.ThreeLegged
    envThreeLegged =
      Maybe.fromJust $
      OAuthThreeL.parseThreeLegged
        "https://api.twitter.com/oauth/request_token"
        "https://api.twitter.com/oauth/authorize"
        "https://api.twitter.com/oauth/access_token"
        (OAuthThreeL.Callback (fromString (envBaseUrl <> envCallbackPath)))

-- | TODO: maybe add some expiry logic, like a delayed-deletion
rememberRequestToken :: RequestTokenCache -> Token Temporary -> IO ()
rememberRequestToken cache tok =
  STM.atomically . STM.modifyTVar' cache $ Map.insert (tok ^. OAuthCred.key) tok

forgetRequestToken :: RequestTokenCache -> OAuthCred.Key -> IO ()
forgetRequestToken cache key =
  STM.atomically . STM.modifyTVar' cache $ Map.delete key

relativeUrl :: (IsString s, Monoid s) => s -> s
relativeUrl = ("https://api.twitter.com/1.1/" <>)

myOAuthServer :: OAuthParams.Server
myOAuthServer =
  OAuthParams.Server
    OAuthParams.AuthorizationHeader
    OAuthParams.HmacSha1
    OAuthParams.OAuth1

data StartOAuthFlowResult
  = OAuthRequestTokenError BSL.ByteString
  | OAuthRedirectToAuthorisationPage TL.Text

-- | Lower-level action
startOAuthFlow' :: Env -> IO StartOAuthFlowResult
startOAuthFlow' UnsafeEnv { envBaseUrl = selfServerBaseUrl
                          , envRequestTokenCache = cache
                          , envClientCred = clientCred
                          , envHttpManager = man
                          , envThreeLegged
                          } = do
  let apparentlyNecessary =
        myOAuthServer {OAuthParams.parameterMethod = OAuthParams.QueryString}
  eitherRequestToken <-
    HttpClient.responseBody <$>
    OAuthThreeL.requestTemporaryToken
      clientCred
      apparentlyNecessary
      envThreeLegged
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
              envThreeLegged
          url = TL.pack (show authorizeUrl)
      putStrLn ("Redirecting to URL: " ++ show url)
      return (OAuthRedirectToAuthorisationPage url)

data ExchangeAccessTokenResult
  = AccessTokenDenied BSL.ByteString
  | AccessTokenAcquired (Token Permanent)

forfeitRequestToken :: Env -> OAuthCred.Key -> IO ()
forfeitRequestToken UnsafeEnv {envRequestTokenCache = cache} requestTokenKey = do
  forgetRequestToken cache requestTokenKey

-- | Lower-level action
handleOAuthCallback' ::
     Env
  -> (OAuthCred.Key, OAuthThreeL.Verifier)
  -> IO ExchangeAccessTokenResult
handleOAuthCallback' UnsafeEnv { envBaseUrl = selfServerBaseUrl
                               , envRequestTokenCache = cache
                               , envClientCred = clientCred
                               , envHttpManager = httpMan
                               , envThreeLegged
                               } (reqTokenKey, verifierBs) = do
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
         envThreeLegged
         httpMan)
  return $
    case HttpClient.responseBody result of
      Left bs -> AccessTokenDenied bs
      Right token -> AccessTokenAcquired token
