{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module TwitAnalysis.ReCaptcha where

import qualified Data.Aeson as A
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Network.HTTP.Client as HC
import qualified Network.HTTP.Types as HT
import qualified System.Environment as Sys

import TwitAnalysis.HttpUtils (withJsonResponse)

data Env =
  UnsafeEnv
    { envHttpMan :: HC.Manager
    , envGoogleRecaptchaApiKey :: T.Text
    , envGoogleRecaptchaApiSecret :: T.Text
    }

newEnv :: HC.Manager -> IO Env
newEnv envHttpMan = do
  key <- Sys.lookupEnv "GOOGLE_RECAPTCHA_KEY"
  sec <- Sys.lookupEnv "GOOGLE_RECAPTCHA_SECRET"
  case (key, sec) of
    (Just k, Just s) -> do
      let envGoogleRecaptchaApiKey = T.pack k
          envGoogleRecaptchaApiSecret = T.pack s
      return
        UnsafeEnv
          {envHttpMan, envGoogleRecaptchaApiKey, envGoogleRecaptchaApiSecret}
    _ ->
      fail $
      "Need GOOGLE_RECAPTCHA_KEY and GOOGLE_RECAPTCHA_SECRET: " ++
      show (key, sec)

-- | Exported data-type
data Result
  = Rejected
  | Ok

data GoogleErrorCode
  = GoogleMissingInputSecret
  | GoogleInvalidInputSecret
  | GoogleMissingInputResponse
  | GoogleInvalidInputResponse
  | GoogleBadRequest
  | GoogleTimeoutOrDuplicate
  deriving (Show, Eq)

-- | Modelling Google's API reponse
--
-- Docs here:
--    https://developers.google.com/recaptcha/docs/verify
data GoogleResponse =
  GoogleResponse
    { grSuccess :: Bool
    , grTimestamp :: Maybe T.Text -- ISO-8601
    , grHostname :: Maybe T.Text
    , grErrorCodes :: [GoogleErrorCode]
    }
  deriving (Show, Eq)

instance A.FromJSON GoogleResponse where
  parseJSON =
    A.withObject "Google ReCaptcha response" $ \o -> do
      grSuccess <- o A..: "success"
      grTimestamp <- o A..:? "challenge_ts"
      grHostname <- o A..:? "hostname"
      grErrorCodes <- o A..:! "error-codes" A..!= []
      return GoogleResponse {grSuccess, grTimestamp, grHostname, grErrorCodes}

instance A.FromJSON GoogleErrorCode where
  parseJSON =
    A.withText "Google ReCaptcha response error-code" $ \case
      "missing-input-secret" -> return GoogleMissingInputSecret
      "invalid-input-secret" -> return GoogleInvalidInputSecret
      "missing-input-response" -> return GoogleMissingInputResponse
      "invalid-input-response" -> return GoogleInvalidInputResponse
      "bad-request" -> return GoogleBadRequest
      "timeout-or-duplicate" -> return GoogleTimeoutOrDuplicate
      other ->
        fail $ "Unrecognised error from the Google API: " ++ T.unpack other

-- | https://developers.google.com/recaptcha/docs/verify
verifyCaptcha :: Env -> BS.ByteString -> IO Result
verifyCaptcha UnsafeEnv {envHttpMan, envGoogleRecaptchaApiSecret} response = do
  initReq <- HC.parseRequest "https://www.google.com/recaptcha/api/siteverify"
  let req =
        HC.urlEncodedBody
          [ ("secret", TE.encodeUtf8 envGoogleRecaptchaApiSecret)
          , ("response", response)
          ] $
        initReq {HC.method = "POST"}
  withJsonResponse req envHttpMan $ \resp ->
    case grSuccess (HC.responseBody resp) of
      True -> return Ok
      False -> return Rejected
