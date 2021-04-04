{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}

-- | Like ButlerDemo, but the real deal
module TwitAnalysis.LoginFlow
  ( Env
  , newEnv
  , handleOAuthCallback
  , handleLogin
  , handleLogout
  , viewHome
  , sessionAccessToken
  , sessionAccessCred
  ) where

import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy.Char8 as BSL8
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import Data.String (IsString, fromString)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Lens.Micro ((^.))
import qualified Network.HTTP.Client as HC
import qualified Network.HTTP.Types as HT
import qualified Network.HTTP.Types.Status as HTS
import qualified Network.OAuth.ThreeLegged as OAuthThreeL
import Network.OAuth.Types.Credentials (Cred, Permanent, Token(Token))
import qualified Network.OAuth.Types.Credentials as OAuthCred
import Web.Scotty (ActionM)
import qualified Web.Scotty as Scotty
import qualified Web.Scotty.Session as Session

import qualified TwitAnalysis.OAuth.AuthFlow as Auth
import qualified TwitAnalysis.ReCaptcha as ReCaptcha
import TwitAnalysis.Utils (mintercalate)

-- | The data stored per-user, in session storage
data UserSession =
  UserSession
    { userSessionAccessToken :: Token Permanent
    }
  deriving (Eq, Show)

data Env =
  UnsafeEnv
    { envSessionMan :: Session.ScottySM UserSession
    }

newtype HomePagePath =
  HomePagePath String

newEnv :: IO Env
newEnv = do
  envSessionMan <- Session.createSessionManager
  return UnsafeEnv {envSessionMan}

sessionAccessToken :: Env -> Scotty.ActionM (Maybe (Token Permanent))
sessionAccessToken UnsafeEnv {envSessionMan} =
  (fmap . fmap) userSessionAccessToken (Session.readSession envSessionMan)

sessionAccessCred ::
     Env -> Cred OAuthCred.Client -> Scotty.ActionM (Maybe (Cred Permanent))
sessionAccessCred env clientCred = do
  maybeToken <- sessionAccessToken env
  return $ do
    token <- maybeToken
    return (OAuthCred.permanentCred token clientCred)

handleLogin :: Env -> Auth.Env -> ReCaptcha.Env -> String -> Scotty.ActionM ()
handleLogin UnsafeEnv {envSessionMan} authEnv recaptchaEnv homePagePath = do
  maybeSession <- Session.readSession envSessionMan
  case maybeSession of
    Nothing -> do
      response <- Scotty.param "g-recaptcha-response"
      liftIO (ReCaptcha.verifyCaptcha recaptchaEnv response) >>= \case
        ReCaptcha.Rejected -> do
          Scotty.status HT.status400 {HT.statusMessage = "ReCaptcha failed"}
          Scotty.html "You didn't pass ReCaptcha."
        ReCaptcha.Ok ->
          liftIO (Auth.startOAuthFlow' authEnv) >>= \case
            Auth.OAuthRequestTokenError bs -> Scotty.raw bs
            Auth.OAuthRedirectToAuthorisationPage url -> Scotty.redirect url
    Just token
      -- user appears already logged-in
     -> do
      Scotty.redirect (fromString homePagePath)

handleLogout :: Env -> String -> Scotty.ActionM ()
handleLogout UnsafeEnv {envSessionMan} homePagePath = do
  Session.modifySession envSessionMan (const Nothing)
  Scotty.redirect (fromString homePagePath)

handleOAuthCallback :: Env -> Auth.Env -> String -> ActionM ()
handleOAuthCallback UnsafeEnv {envSessionMan} authEnv homePagePath = do
  allParams <- Scotty.params
  let denied = List.lookup "denied" allParams
  case denied of
    Nothing -> return ()
    Just requestTokenTl ->
      case Scotty.parseParam requestTokenTl of
        Left err -> renderError (Just "Error from Twitter's redirect")
        Right rtkey -> do
          liftIO $ Auth.forfeitRequestToken authEnv rtkey
          renderError (Just "You denied us OAuth access.")
          Scotty.finish
  reqTokenKey :: OAuthCred.Key <- Scotty.param "oauth_token"
  verifierBs :: OAuthThreeL.Verifier <- Scotty.param "oauth_verifier"
  result <- liftIO $ Auth.handleOAuthCallback' authEnv (reqTokenKey, verifierBs)
  case result of
    Auth.AccessTokenDenied lbs -> do
      liftIO $ do
        putStrLn
          ("Failed to swap request token for access token. Request body printed below.")
        BSL8.putStrLn lbs
      Session.modifySession envSessionMan (const Nothing)
      renderError Nothing
    Auth.AccessTokenAcquired token -> do
      liftIO
        (putStrLn
           ("Successfully received access token: " ++
            show (token ^. OAuthCred.key)))
      Session.modifySession envSessionMan (const (Just (UserSession token)))
      Scotty.redirect (fromString homePagePath)
  where
    renderError :: Maybe String -> Scotty.ActionM ()
    renderError maybeMsg = do
      let defaultMsg = "Failed to obtain access token."
          msg = Maybe.fromMaybe defaultMsg maybeMsg
      Scotty.status HTS.status401 {HTS.statusMessage = BS8.pack msg}
      Scotty.html . TL.fromStrict . T.pack $ msg

viewHome :: String -> Env -> ActionM ()
viewHome loginPath UnsafeEnv {envSessionMan} = do
  maybeToken <- Session.readSession envSessionMan
  case maybeToken of
    Nothing -> Scotty.redirect (fromString loginPath)
    Just token -> do
      Scotty.html $
        mintercalate
          "\n"
          [ "<h1>Welcome!</h1>"
          , "<p>Here is your access token:</p>"
          , "<pre>"
          , TL.pack (show token)
          , "</pre>"
          ]
