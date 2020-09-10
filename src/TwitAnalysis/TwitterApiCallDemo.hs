{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module TwitAnalysis.TwitterApiCallDemo
  ( Env
  , newEnv
  , handleCurrentUser
  , showTweetPrompt
  , handleTweetSubmit
  , withEntireResponse
  ) where

import Control.Applicative ((<|>))
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.IO.Class (MonadIO)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encode.Pretty as AP
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.List as List
import Data.String (fromString)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import qualified Network.HTTP.Client as HC
import qualified Network.HTTP.Client.TLS as Tls
import qualified Network.HTTP.Types.Status as HS
import qualified Network.OAuth as OAuth
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
import qualified TwitAnalysis.OAuth.Signing as MySigning
import TwitAnalysis.Utils (mintercalate)

data Env =
  UnsafeEnv
    { envHttpMan :: HC.Manager
    }

newEnv :: HC.Manager -> IO Env
newEnv envHttpMan = return (UnsafeEnv {envHttpMan})

-- | Help... gather everything into one BSL.ByteString ...
--
-- HELP!! Wanting monad-control MonadBaseControl here but I don't know how to use it.
withEntireResponse ::
     HC.Request -> HC.Manager -> (HC.Response BSL.ByteString -> IO a) -> IO a
withEntireResponse req man cont =
  HC.withResponse req man $ \response -> do
    chunks :: [BS.ByteString] <- HC.brConsume . HC.responseBody $ response
    cont (fmap (const (BSL.fromChunks chunks)) response)

withResponseLbs :: HC.Request -> HC.Manager -> (BSL.ByteString -> IO a) -> IO a
withResponseLbs req man cont =
  withEntireResponse req man (cont . HC.responseBody)

-- | Early-half of request handling
fetchCurrentUser ::
     Env -> Cred Permanent -> OAuthParams.Server -> IO Aeson.Value
fetchCurrentUser UnsafeEnv {envHttpMan} cred srv = do
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
  oauthParams <- MySigning.genInitOAuthParams cred
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
    case Aeson.decode lbs of
      Nothing -> fail "Failed to decode response from Twitter API as JSON"
      Just v -> return v

viewUser :: Aeson.Value -> Markup
viewUser json = do
  H.html $
    H.body $ do
      H.h1 "Your Twitter user is shown below:"
      H.pre . H.toHtml . TLE.decodeUtf8 . AP.encodePretty $ json

handleCurrentUser :: Env -> Auth.Env -> Login.Env -> String -> Scotty.ActionM ()
handleCurrentUser env authEnv loginEnv loginPath = do
  maybeAccessToken <- Login.sessionAccessToken loginEnv
  case maybeAccessToken of
    Nothing -> Scotty.redirect (fromString loginPath)
    Just accessToken -> do
      liftIO . putStrLn $ "Using this access token: " ++ show accessToken
      let accessCred :: Cred Permanent =
            OAuthCred.permanentCred accessToken (Auth.envClientCred authEnv)
      liftIO . putStrLn $ "Using this access cred: " ++ show accessCred
      json :: Aeson.Value <-
        liftIO $ fetchCurrentUser env accessCred Auth.myOAuthServer
      Scotty.html . BlazeT.renderHtml $ viewUser json

viewTweetPrompt :: H.AttributeValue -> Markup
viewTweetPrompt submitUrl =
  H.html $
  H.body $ do
    H.h1 "Send a tweet here."
    H.form ! A.action submitUrl ! A.method "POST" $ do
      H.textarea ! A.name "tweet-content" $ H.text ""
      H.input ! A.type_ "submit" ! A.value "send tweet"

showTweetPrompt :: Login.Env -> String -> String -> Scotty.ActionM ()
showTweetPrompt loginEnv loginPath submitUrl = do
  Login.sessionAccessToken loginEnv >>= \case
    Nothing -> Scotty.redirect (fromString loginPath)
    Just _ ->
      Scotty.html . BlazeT.renderHtml . viewTweetPrompt . H.stringValue $
      submitUrl

-- | POST statuses/update
--
--    https://developer.twitter.com/en/docs/twitter-api/v1/tweets/post-and-engage/api-reference/post-statuses-update
sendTweet ::
     HC.Manager
  -> Cred Permanent
  -> OAuth.Server
  -> T.Text
  -> IO (HC.Response BSL.ByteString)
sendTweet httpMan accessCred srv content = do
  hreq0 <- HC.parseRequest "https://api.twitter.com/1.1/statuses/update.json"
  let hreq = HC.urlEncodedBody [("status", TE.encodeUtf8 content)] hreq0
  signed <- MySigning.oauth accessCred srv hreq
  withEntireResponse signed httpMan return

viewSentTweet :: BSL.ByteString -> Markup
viewSentTweet bsl =
  case Aeson.decode bsl of
    Nothing ->
      H.html $
      H.body $ do
        H.h1 "Error parsing send-tweet response:"
        H.pre $ H.lazyText (TLE.decodeUtf8 bsl)
    Just (j :: Aeson.Value) ->
      let t :: TL.Text = TLE.decodeUtf8 $ AP.encodePretty j
       in H.html $
          H.body $ do
            H.h1 "Here is the tweet you've sent:"
            H.pre (H.lazyText t)

viewSentTweetError :: BSL.ByteString -> Markup
viewSentTweetError bsl =
  H.html $
  H.body $ do
    H.h1 "Something went wrong when sending your tweet"
    case Aeson.decode bsl of
      Nothing -> do
        H.p "Cannot parse send-tweet response:"
        H.pre (H.lazyText (TLE.decodeUtf8 bsl))
      Just (j :: Aeson.Value) -> do
        H.p "Error:"
        H.pre (H.lazyText (TLE.decodeUtf8 (AP.encodePretty j)))

handleTweetSubmit :: Env -> Auth.Env -> Login.Env -> String -> Scotty.ActionM ()
handleTweetSubmit UnsafeEnv {envHttpMan} authEnv loginEnv loginPath = do
  Login.sessionAccessToken loginEnv >>= \case
    Nothing -> Scotty.redirect (fromString loginPath)
    Just accessToken -> do
      tweetContent :: T.Text <- Scotty.param "tweet-content" -- read form submission
      let accessCred =
            OAuthCred.permanentCred accessToken (Auth.envClientCred authEnv)
      response <-
        liftIO $ sendTweet envHttpMan accessCred Auth.myOAuthServer tweetContent
      if HS.statusIsSuccessful (HC.responseStatus response)
        then Scotty.html . BlazeT.renderHtml . viewSentTweet . HC.responseBody $
             response
        else Scotty.html .
             BlazeT.renderHtml . viewSentTweetError . HC.responseBody $
             response
