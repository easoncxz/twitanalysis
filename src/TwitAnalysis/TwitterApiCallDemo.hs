{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module TwitAnalysis.TwitterApiCallDemo where

import Control.Applicative ((<|>))
import Control.Monad.IO.Class (liftIO)
import qualified Data.Aeson as A
import qualified Data.Aeson.Encode.Pretty as AP
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.List as List
import Data.String (fromString)
import qualified Data.Text.Lazy.Encoding as TL
import qualified Network.HTTP.Client as HC
import qualified Network.HTTP.Client.TLS as Tls
import qualified Network.OAuth.Signing as OAuthSigning
import Network.OAuth.Types.Credentials (Cred, Permanent)
import qualified Network.OAuth.Types.Credentials as OAuthCred
import qualified Network.OAuth.Types.Params as OAuthParams
import qualified Text.Blaze.Html.Renderer.Text as BlazeT
import Text.Blaze.Html5 (Markup, (!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Web.Scotty as Scotty
import qualified Web.Scotty.Session as Session

import qualified TwitAnalysis.LoginFlow as Login
import qualified TwitAnalysis.OAuth.AuthFlow as Auth
import qualified TwitAnalysis.OAuth.Signing as Signing
import TwitAnalysis.Utils (mintercalate)

data Env =
  Env
    { envHttpMan :: HC.Manager
    }

-- | Help... gather everything into one BSL.ByteString ...
withResponseLbs :: HC.Request -> HC.Manager -> (BSL.ByteString -> IO a) -> IO a
withResponseLbs req man cont =
  HC.withResponse req man $ \response -> do
    chunks :: [BS.ByteString] <- HC.brConsume . HC.responseBody $ response
    cont (BSL.fromChunks chunks)

-- | Early-half of request handling
fetchCurrentUser :: Env -> Cred Permanent -> OAuthParams.Server -> IO A.Value
fetchCurrentUser Env {envHttpMan} cred srv = do
  initReq <-
    HC.parseRequest
      "GET https://api.twitter.com/1.1/account/verify_credentials.json"
  let unsignedReq =
        HC.setQueryString
          [ ("include_entities", Just "true")
          , ("skip_status", Just "false")
          , ("include_email", Just "false")
          ]
          initReq
  oauthParams <- Signing.genInitOAuthParams cred
  putStrLn $ "Here are the generated OAuth params: " ++ show oauthParams
  let signedReq = OAuthSigning.sign oauthParams srv unsignedReq
  putStrLn $ "About to make this request: " ++ show signedReq
  let headers = (HC.requestHeaders signedReq)
  putStrLn $ "In particular, the headers are: " ++ show headers
  let authHeader =
        List.lookup "Authorization" headers <|>
        List.lookup "authorization" headers
  putStrLn $ "The Authorization header is: " ++ show authHeader
  withResponseLbs signedReq envHttpMan $ \lbs -> do
    case A.decode lbs of
      Nothing -> fail "Failed to decode response from Twitter API as JSON"
      Just v -> return v

viewUser :: A.Value -> Markup
viewUser json = do
  H.html $
    H.body $ do
      H.h1 "Your Twitter user is shown below:"
      H.pre . H.toHtml . TL.decodeUtf8 . AP.encodePretty $ json

handleCurrentUser :: Env -> Auth.Env -> Login.Env -> String -> Scotty.ActionM ()
handleCurrentUser env authEnv loginEnv loginPath = do
  maybeAccessToken <-
    (fmap . fmap)
      Login.userSessionAccessToken
      (Session.readSession (Login.envSessionMan loginEnv))
  case maybeAccessToken of
    Nothing -> Scotty.redirect (fromString loginPath)
    Just accessToken -> do
      liftIO . putStrLn $ "Using this access token: " ++ show accessToken
      let accessCred :: Cred Permanent =
            OAuthCred.permanentCred accessToken (Auth.envClientCred authEnv)
      liftIO . putStrLn $ "Using this access cred: " ++ show accessCred
      json :: A.Value <-
        liftIO $ fetchCurrentUser env accessCred Auth.myOAuthServer
      Scotty.html . BlazeT.renderHtml $ viewUser json
