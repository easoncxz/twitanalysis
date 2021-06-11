{-# LANGUAGE OverloadedStrings #-}

-- | Signing requests
--
-- This is one-layer higher than the neighbour `AuthFlow` module.
module TwitAnalysis.OAuth.Signing where

import Control.Monad.IO.Class (MonadIO)
import Crypto.Random (MonadRandom)
import qualified Data.ByteString as B
import qualified Network.HTTP.Client as HC
import qualified Network.OAuth.Signing as OAuthSigning
import Network.OAuth.Types.Credentials
  ( Client
  , Cred
  , Permanent
  , Temporary
  , Token(Token)
  )
import qualified Network.OAuth.Types.Credentials as OAuthCred
import qualified Network.OAuth.Types.Params as OAuthParams

import qualified TwitAnalysis.OAuth.AuthFlow as AuthFlow

-- | Basically OAuthParams.freshOa, but with my hack applied
genInitOAuthParams ::
     (MonadIO m, MonadRandom m)
  => Cred Permanent
  -> m (OAuthParams.Oa Permanent)
genInitOAuthParams accessCred = do
  oax <- OAuthParams.freshOa accessCred
  -- Hack around a difference-in-understanding compared to the author of oauthenticated.
  -- The `oauth_token` component would be omitted from the `Authorization` header otherwise.
  -- See journal for back-story:
  --  - https://github.com/easoncxz/twitanalysis/wiki/journal-2020-09-08:-A-%22bug%22-in-%60oauthenticated%60
  let hackedOax :: OAuthParams.Oa Permanent
      hackedOax =
        oax {OAuthParams.workflow = OAuthParams.PermanentTokenRequest "bogus"}
  return hackedOax

-- | Combining `OAuthParams.freshOa` and `OAuthParams.sign`
--
-- Basically `OAuthSigning.oauth`.
oauth ::
     (MonadIO m, MonadRandom m)
  => Cred Permanent
  -> OAuthParams.Server
  -> HC.Request
  -> m HC.Request
oauth accessCred srv hreq = do
  oax <- genInitOAuthParams accessCred
  return $ OAuthSigning.sign oax srv hreq

adhocSign ::
     (B.ByteString, B.ByteString)
  -> (B.ByteString, B.ByteString)
  -> HC.Request
  -> IO HC.Request
adhocSign (consumerKey, consumerSec) (accessTok, accessSec) hreq = do
  let clientCred =
        OAuthCred.clientCred (OAuthCred.Token consumerKey consumerSec)
      permanentCred =
        OAuthCred.permanentCred (OAuthCred.Token accessTok accessSec) clientCred
      srv =
        OAuthParams.Server
          OAuthParams.AuthorizationHeader
          OAuthParams.HmacSha1
          OAuthParams.OAuth1
  oauth permanentCred srv hreq
