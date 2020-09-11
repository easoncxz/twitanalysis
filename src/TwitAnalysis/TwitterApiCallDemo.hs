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

import qualified Blaze.ByteString.Builder as BlazeB
import Control.Applicative ((<|>))
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.IO.Class (MonadIO)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encode.Pretty as AP
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Conduit as U
import qualified Data.List as List
import Data.String (fromString)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import qualified Network.HTTP.Client as HC
import qualified Network.HTTP.Client.Conduit as HCU
import qualified Network.HTTP.Client.TLS as Tls
import qualified Network.HTTP.Types.Status as HS
import qualified Network.OAuth as OAuth
import qualified Network.OAuth.Signing as OAuthSigning
import Network.OAuth.Types.Credentials (Cred, Permanent)
import qualified Network.OAuth.Types.Credentials as OAuthCred
import qualified Network.OAuth.Types.Params as OAuthParams
import qualified Network.Wai.Conduit as WU
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
    let bsl = BSL.fromChunks chunks
    cont (const bsl <$> response)

withResponseLbs :: HC.Request -> HC.Manager -> (BSL.ByteString -> IO a) -> IO a
withResponseLbs req man cont =
  withEntireResponse req man (cont . HC.responseBody)

-- | Early-half of request handling
fetchCurrentUser ::
     Env -> Cred Permanent -> OAuthParams.Server -> IO Aeson.Value
fetchCurrentUser UnsafeEnv {envHttpMan} cred srv = do
  initReq <-
    HC.parseRequest
      "https://api.twitter.com/1.1/account/verify_credentials.json"
  let unsignedReq =
        HC.setQueryString
          [ ("include_entities", Just "true")
          , ("skip_status", Just "false")
          , ("include_email", Just "false")
          ]
          initReq {HC.method = "GET"}
  putStrLn $
    "fetchCurrentUser about to sign this HC.Request: " ++ show unsignedReq
  oauthParams <- MySigning.genInitOAuthParams cred
  putStrLn $ "Here are the generated OAuth params: " ++ show oauthParams
  let signedReq = OAuthSigning.sign oauthParams srv unsignedReq
  let headers = (HC.requestHeaders signedReq)
  let authHeader =
        List.lookup "Authorization" headers <|>
        List.lookup "authorization" headers
  putStrLn $
    "fetchCurrentUser about to make this HC.Request: " ++ show signedReq
  putStrLn $ "In particular, the headers are: " ++ show headers
  putStrLn $ "The Authorization header is: " ++ show authHeader
  withEntireResponse signedReq envHttpMan $ \response -> do
    let lbs = HC.responseBody response
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
--
-- Example Wai.Request:
--
--    Request
--      { requestMethod = "GET"
--      , httpVersion = HTTP/1.1
--      , rawPathInfo = "/butler"
--      , rawQueryString = "?bogus-param=42"
--      , requestHeaders =
--          [ ("Host", "localhost:5000")
--          , ("User-Agent", "Mozilla/5.0 (Macintosh; Intel Mac OS X 10.13; rv:79.0) Gecko/20100101 Firefox/79.0")
--          , ("Accept", "text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,*/*;q=0.8")
--          , ("Accept-Language", "en-US,en;q=0.5")
--          , ("Accept-Encoding", "gzip, deflate")
--          , ("DNT", "1")
--          , ("Connection", "keep-alive")
--          , ("Upgrade-Insecure-Requests", "1")
--          ]
--      , isSecure = False
--      , remoteHost = 127.0.0.1:61998
--      , pathInfo = ["butler"]
--      , queryString = [("bogus-param",Just "42")]
--      , requestBody = <IO ByteString>
--      , vault = <Vault>
--      , requestBodyLength = KnownLength 0
--      , requestHeaderHost = Just "localhost:5000"
--      , requestHeaderRange = Nothing
--      }
--
-- Example HC.Request made by this sendTweet function:
--
--    sendTweet about to make this HC.Request: Request {
--        host                 = "api.twitter.com"
--        port                 = 443
--        secure               = True
--        requestHeaders       = [("Content-Type","application/x-www-form-urlencoded"),("Authorization","<REDACTED>")]
--        path                 = "/1.1/statuses/update.json"
--        queryString          = ""
--        method               = "POST"
--        proxy                = Nothing
--        rawBody              = False
--        redirectCount        = 10
--        responseTimeout      = ResponseTimeoutDefault
--        requestVersion       = HTTP/1.1
--      }
--
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
  putStrLn ("sendTweet about to make this HC.Request: " ++ show signed)
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

viewSentTweetError :: HC.Response BSL.ByteString -> Markup
viewSentTweetError resp =
  let bsl = HC.responseBody resp
   in H.html $
      H.body $ do
        H.h1 "Something went wrong when sending your tweet"
        case Aeson.decode bsl of
          Nothing -> do
            H.p "Cannot parse send-tweet response:"
            H.pre (H.lazyText (TLE.decodeUtf8 bsl))
          Just (j :: Aeson.Value) -> do
            H.p "Error:"
            H.p $
              H.text (T.pack ("Status code: " ++ show (HC.responseStatus resp)))
            H.pre (H.lazyText (TLE.decodeUtf8 (AP.encodePretty j)))
            H.h2 "The entire HC.Request is shown below:"
            H.pre (H.text (T.pack (show resp)))

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
        else Scotty.html . BlazeT.renderHtml . viewSentTweetError $ response
