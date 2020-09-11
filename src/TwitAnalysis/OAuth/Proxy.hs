{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Building blocks for defining my "OAuth proxy" idea:
-- receive unsigned requests, and sign them.
--
-- Composition of dependencies from:
--
--  - Network.HTTP.Proxy.Patched: doUpstreamRequestVia
--  - TwitAnalysis.OAuth.Signing: sign
module TwitAnalysis.OAuth.Proxy
  ( handlePassthruEndpoint
  -- , requestConversionLoggingMiddleware
  , viewOneParticularTweet
  ) where

import qualified Control.Exception as X
import Control.Monad (forM_)
import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Crypto.Random (MonadRandom)
import Data.Aeson ((.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encode.Pretty as AP
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as BSL8
import Data.CaseInsensitive (CI)
import qualified Data.CaseInsensitive as CI
import Data.String (fromString)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import qualified Network.HTTP.Client as HC
import qualified Network.HTTP.Conduit as HU
import qualified Network.HTTP.Proxy.Patched as HP
import qualified Network.HTTP.Types as HT
import qualified Network.HTTP.Types.Header as HHT
import Network.OAuth.Types.Credentials (Cred, Permanent)
import qualified Network.OAuth.Types.Credentials as OAuthCred
import qualified Network.OAuth.Types.Params as OAuthParams
import qualified Network.Wai as Wai
import qualified Network.Wai.Conduit as WU
import qualified Text.Blaze.Html.Renderer.Text as BlazeT
import Text.Blaze.Html5 (Markup, (!))
import qualified Web.Scotty as Scotty

import qualified TwitAnalysis.LoginFlow as Login
import TwitAnalysis.OAuth.Proxy.Internals
  ( parsePathComps
  , stripPathPrefix
  , stripRequestPathPrefix
  , withUtf8
  )
import qualified TwitAnalysis.OAuth.Signing as MySigning
import qualified TwitAnalysis.TwitterApiCallDemo as ApiCallDemo

-- waiRequestToHttpClient ::
--      (MonadThrow m, MonadIO m) => Wai.Request -> m HC.Request
-- waiRequestToHttpClient wreq = do
--   hreq0 <-
--     let toBeOverwritten = "https://api.twitter.com" :: BS.ByteString
--      in HC.parseRequest $
--         BS8.unpack
--           (toBeOverwritten <> Wai.rawPathInfo wreq <> Wai.rawQueryString wreq)
--   let body = convertBody wreq
--   let hreq1 =
--         hreq0
--           { HC.method = Wai.requestMethod wreq
--           , HC.requestHeaders =
--               HC.requestHeaders hreq0 ++
--               (filter keepHeader (Wai.requestHeaders wreq))
--           , HC.redirectCount = 0 -- Always pass redirects back to the client.
--           , HC.requestBody = body
--               -- HU.requestBodySourceChunkedIO (WU.sourceRequestBody wreq)
--           -- , HC.decompress = const False
--           }
--   return hreq1
--   where
--     keepHeader (k, _) =
--       k `notElem`
--       [ "content-encoding"
--       , "content-length"
--       , "host"
--       , "referer"
--       , "origin"
--       , "cookie"
--       , "user-agent"
--       , "dnt"
--       ]
--
-- requestConversionLoggingMiddleware :: Wai.Middleware
-- requestConversionLoggingMiddleware next request respond = do
--   hreq :: HC.Request <- waiRequestToHttpClient request
--   putStrLn
--     ("requestConversionLoggingMiddleware got this request: " ++ show hreq)
--   next request respond
unused :: Int
unused = undefined

handlePassthruEndpoint ::
     T.Text
  -> HC.Manager
  -> Cred OAuthCred.Client
  -> OAuthParams.Server
  -> Login.Env
  -> Scotty.ActionM ()
handlePassthruEndpoint passthruRoute httpMan clientCred srv loginEnv = do
  liftIO $ putStrLn "Control reached handlePassthruEndpoint"
  maybeAccessCred <- Login.sessionAccessCred loginEnv clientCred
  case maybeAccessCred of
    Nothing -> do
      Scotty.status HT.status401
      let msg = "You are not logged in yet" :: T.Text
      Scotty.json $ Aeson.object ["error" .= msg]
    Just accessCred -> do
      wreq :: Wai.Request <- Scotty.request
      liftIO $ logWaiRequestBody wreq
      signed :: HC.Request <-
        liftIO $ prepareSignedRequest accessCred srv passthruRoute wreq
      jsonResponse :: HC.Response Aeson.Value <- sendJsonRequest httpMan signed
      interpretJsonResponse jsonResponse
  where
    logWaiRequestBody wreq = do
      lbs :: BSL.ByteString <- Wai.strictRequestBody wreq
      BSL8.putStrLn ("logWaiRequestBody: " <> lbs)

prepareSignedRequest ::
     Cred Permanent
  -> OAuthParams.Server
  -> T.Text
  -> Wai.Request
  -> IO HC.Request
prepareSignedRequest accessCred srv passthruRoute wreq = do
  let passthruRouteComps = parsePathComps passthruRoute
      strippedRawPathInfoAndQuery =
        T.unpack $
        stripPathPrefix
          passthruRouteComps
          (TE.decodeUtf8 (Wai.rawPathInfo wreq <> Wai.rawQueryString wreq))
  let url =
        "https://api.twitter.com/1.1" <> strippedRawPathInfoAndQuery :: String
      method = Wai.requestMethod wreq :: BS.ByteString
      -- | Only send a select few headers
      headers =
        pickHeaders
          [ "Content-Type"
          , "Accept"
          -- , "Content-Length"
          -- , "Content-Encoding"
          , "Accept-Encoding"
          ]
          (Wai.requestHeaders wreq) :: [HHT.Header]
  putStrLn ("prepareSignedRequest about to use this URL: " ++ url)
  putStrLn ("prepareSignedRequest about to use this method: " ++ show method)
  putStrLn
    ("prepareSignedRequest about to use these request headers: " ++ show headers)
  body <-
    do bodyLbs <- Wai.strictRequestBody wreq
       BSL8.putStrLn
         ("prepareSignedRequest about to use this request body: " <> bodyLbs)
       return $ HC.RequestBodyLBS bodyLbs
  -- let body = convertBody wreq
  hreq0 <- HC.parseRequest url
  let hreq1 =
        hreq0
          { HC.method = method
          , HC.requestHeaders = headers
          , HC.requestBody = body
          }
  putStrLn
    ("prepareSignedRequest about to sign this unauthenticated HC.Request: " ++
     show hreq1)
  signed <- MySigning.oauth accessCred srv hreq1
  putStrLn
    ("prepareSignedRequest about to present this signed HC.Request: " ++
     show signed)
  putStrLn "In particular, the above HC.Request has these headers: "
  forM_ (HC.requestHeaders signed) $ \(name, value) -> do
    BS8.putStrLn ("HC.Request header: " <> CI.original name <> " = " <> value)
  return signed

-- | Make the request to Twitter (or fail the request to us)
sendJsonRequest ::
     HC.Manager -> HC.Request -> Scotty.ActionM (HC.Response Aeson.Value)
sendJsonRequest httpMan hreq = do
  hresp <- liftIO $ ApiCallDemo.withEntireResponse hreq httpMan return
  j <- scottyDecodeJson (HC.responseBody hresp)
  return (const j <$> hresp)

viewOneParticularTweet :: Scotty.ActionM ()
viewOneParticularTweet = do
  let status = HT.status200
      headers = [(CI.mk "Content-Type", "application/json")]
  bodyLbs :: BSL.ByteString <-
    liftIO $ BSL8.readFile "test/resources/sample-status.json"
  body :: Aeson.Value <- scottyDecodeJson bodyLbs
  interpretJsonResponseComponents status headers body

-- | "It has to decode."
scottyDecodeJson :: BSL.ByteString -> Scotty.ActionM Aeson.Value
scottyDecodeJson bsl =
  case Aeson.eitherDecode bsl of
    Left msg -> do
      Scotty.json $ Aeson.object ["error" .= Aeson.String (T.pack msg)]
      Scotty.finish
    Right (j :: Aeson.Value) -> return j

-- | Pick out pieces of the HC.Response and explain them to Scotty
interpretJsonResponse :: HC.Response Aeson.Value -> Scotty.ActionM ()
interpretJsonResponse res = do
  liftIO $ do
    putStrLn
      ("interpretJsonResponse about to render HC.Response status: " ++
       show status)
    forM_ headers $ \(name, value) -> do
      BS8.putStrLn
        ("interpretJsonResponse about to render HC.Response header: " <>
         CI.original name <> " = " <> value)
    BSL8.putStrLn
      ("interpretJsonResponse about to render HC.Response body: " <>
       AP.encodePretty body)
  interpretJsonResponseComponents status headers body
  where
    status = HC.responseStatus res
    -- | Only render a select few headers
    headers = pickHeaders ["Content-Type"] $ HC.responseHeaders res
    body = HC.responseBody res

interpretJsonResponseComponents ::
     HT.Status -> [HHT.Header] -> Aeson.Value -> Scotty.ActionM ()
interpretJsonResponseComponents status headers bodyJson = do
  Scotty.status status
  -- Really set all headers?
  forM_ headers $ \(name, value) -> do
    Scotty.setHeader
      (TL.fromStrict (TE.decodeUtf8 (CI.original name)))
      (TL.fromStrict (TE.decodeUtf8 value))
  Scotty.json bodyJson

pickHeaders :: Eq k => [k] -> [(k, v)] -> [(k, v)]
pickHeaders keys orig = filter ((`elem` keys) . fst) orig

dropHeaders :: Eq k => [k] -> [(k, v)] -> [(k, v)]
dropHeaders keys orig = filter ((`notElem` keys) . fst) orig

-- | Picking out the Conduit part of `doUpstreamRequest` from `http-proxy`
convertBody :: Wai.Request -> HC.RequestBody
convertBody wreq =
  case Wai.requestBodyLength wreq of
    Wai.ChunkedBody -> HU.requestBodySourceChunkedIO (WU.sourceRequestBody wreq)
    Wai.KnownLength l ->
      HU.requestBodySourceIO (fromIntegral l) (WU.sourceRequestBody wreq)
