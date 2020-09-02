{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}

module Lib where

import Control.Concurrent.STM (TVar)
import qualified Control.Concurrent.STM as STM
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map
import qualified Network.HTTP.Client as Http
import qualified Network.OAuth.Types.Credentials as OAuthCred
import qualified Text.Blaze as Blaze
import qualified Text.Blaze.Html.Renderer.Text as BlazeT
import qualified Web.Scotty as Scotty

import qualified TwitterOAuthConsumer as TwitterO

startOAuthFlow :: TwitterO.AppEnv -> Scotty.ActionM ()
startOAuthFlow e =
  liftIO (TwitterO.startOAuthFlow e) >>= \case
    TwitterO.OAuthRequestTokenError bs -> Scotty.raw bs
    TwitterO.OAuthRedirectToAuthorisationPage url -> Scotty.redirect url

handleOAuthCallback :: TwitterO.AppEnv -> Scotty.ActionM ()
handleOAuthCallback env = do
  reqTokenKey :: BS.ByteString <- Scotty.param "oauth_token"
  verifierBs :: BS.ByteString <- Scotty.param "oauth_verifier"
  (accTokenKey, accTokenSecret) <-
    liftIO $ TwitterO.handleOAuthCallback env reqTokenKey verifierBs
  liftIO (putStrLn ("Successfully received access token: " ++ show accTokenKey))
  Scotty.html $
    TwitterO.mintercalate
      "\n"
      [ "Here is your access token: "
      , "<pre>"
      , BlazeT.renderHtml (Blaze.string (show (accTokenKey, accTokenSecret)))
      , "</pre>"
      ]
