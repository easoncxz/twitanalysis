{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}

-- | Like ButlerDemo, but the real deal
module TwitAnalysis.LoginFlow
  ( Env
  , newEnv
  , handleOAuthCallback
  , viewLogin
  , viewHome
  , sessionAccessToken
  , sessionAccessCred
  ) where

import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy.Char8 as BSL8
import Data.String (IsString, fromString)
import qualified Data.Text.Lazy as TL
import Lens.Micro ((^.))
import qualified Network.HTTP.Types.Status as Http
import qualified Network.OAuth.ThreeLegged as OAuthThreeL
import Network.OAuth.Types.Credentials (Cred, Permanent, Token(Token))
import qualified Network.OAuth.Types.Credentials as OAuthCred
import Web.Scotty (ActionM)
import qualified Web.Scotty as Scotty
import qualified Web.Scotty.Session as Session

import qualified TwitAnalysis.OAuth.AuthFlow as Auth
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

viewLogin :: Env -> Auth.Env -> String -> Scotty.ActionM ()
viewLogin UnsafeEnv {envSessionMan} authEnv homePagePath = do
  maybeSession <- Session.readSession envSessionMan
  case maybeSession of
    Nothing ->
      liftIO (Auth.startOAuthFlow' authEnv) >>= \case
        Auth.OAuthRequestTokenError bs -> Scotty.raw bs
        Auth.OAuthRedirectToAuthorisationPage url -> Scotty.redirect url
    Just token
      -- user appears already logged-in
     -> do
      Scotty.redirect (fromString homePagePath)

handleOAuthCallback :: Env -> Auth.Env -> String -> ActionM ()
handleOAuthCallback UnsafeEnv {envSessionMan} authEnv homePagePath = do
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
      renderError
    Auth.AccessTokenAcquired token -> do
      liftIO
        (putStrLn
           ("Successfully received access token: " ++
            show (token ^. OAuthCred.key)))
      Session.modifySession envSessionMan (const (Just (UserSession token)))
      Scotty.redirect (fromString homePagePath)
  where
    renderError = do
      let msg :: IsString s => s
          msg = "Failed to obtain access token."
      Scotty.status Http.status401 {Http.statusMessage = msg}
      Scotty.html msg

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