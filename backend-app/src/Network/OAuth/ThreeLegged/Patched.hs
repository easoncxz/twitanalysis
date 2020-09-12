{-# LANGUAGE NamedFieldPuns #-}

-- | Extracted some functions from the source for `Network.OAuth.ThreeLegged`
module Network.OAuth.ThreeLegged.Patched where

import Control.Monad.IO.Class (MonadIO, liftIO)

import qualified Data.ByteString.Lazy as BSL
import qualified Network.HTTP.Client as Http
import qualified Network.OAuth as OAuth
import qualified Network.OAuth.ThreeLegged as OAuthThreeL
import qualified Network.OAuth.Types.Credentials as OAuthCred
import qualified Network.OAuth.Types.Params as OAuthParams

-- | The first-third of the logic of `requestTemporaryToken`
--
-- Builds a Http.Request but does not perform it. This allows you to
-- inspect the request before sending it.
makeTemporaryTokenRequest ::
     (MonadIO m)
  => OAuthCred.Cred OAuth.Client
  -> OAuth.Server
  -> OAuthThreeL.ThreeLegged
  -> Http.Manager
  -> m Http.Request
makeTemporaryTokenRequest cr srv three man =
  liftIO $ do
    let OAuthThreeL.ThreeLegged { OAuthThreeL.temporaryTokenRequest
                                , OAuthThreeL.callback
                                } = three
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

-- | The last-third of `requestTemporaryToken`:
--
-- Takes a HTTP response body, and tries to parse an OAuth (Access) token
-- out of it.
tryParseToken ::
     OAuth.Server -> BSL.ByteString -> Either BSL.ByteString (OAuth.Token ty)
tryParseToken srv lbs =
  case maybeParseToken lbs of
    Nothing -> Left lbs
    Just tok -> Right tok
  where
    maybeParseToken lbs = do
      (confirmed, tok) <- OAuthCred.fromUrlEncoded $ BSL.toStrict lbs
      case OAuthParams.oAuthVersion srv of
        OAuth.OAuthCommunity1 -> return tok
        _ ->
          if confirmed
            then return tok
            else fail "Must be confirmed"

-- | Like requestTemporaryToken, but with an extra first-position param that allows looking at
-- and possibly changing the request before sending it
requestTemporaryTokenInspected ::
     (MonadIO m)
  => (Http.Request -> m Http.Request)
  -> OAuthCred.Cred OAuth.Client
  -> OAuth.Server
  -> OAuthThreeL.ThreeLegged
  -> Http.Manager
  -> m (Either BSL.ByteString (OAuthCred.Token OAuth.Temporary))
requestTemporaryTokenInspected inspect cr srv three man = do
  request <- inspect =<< makeTemporaryTokenRequest cr srv three man
  resp <- liftIO (Http.httpLbs request man)
  return (tryParseToken srv (Http.responseBody resp))

-- | Like requestTemporaryToken, but prints `show request` to
-- stdout before sending it off.
--
-- This is an example usage of `requestTemporaryTokenInspected`.
requestTemporaryTokenTraced ::
     (MonadIO m)
  => OAuthCred.Cred OAuth.Client
  -> OAuth.Server
  -> OAuthThreeL.ThreeLegged
  -> Http.Manager
  -> m (Either BSL.ByteString (OAuthCred.Token OAuth.Temporary))
requestTemporaryTokenTraced =
  requestTemporaryTokenInspected $ \request -> do
    liftIO $ putStr "Using the following request for request token: "
    liftIO $ putStrLn (show request)
    return request
