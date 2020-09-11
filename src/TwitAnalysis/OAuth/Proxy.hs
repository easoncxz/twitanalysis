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
  , asMiddleware
  , requestConversionLoggingMiddleware
  ) where

import qualified Control.Exception as X
import Control.Monad (forM_)
import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Crypto.Random (MonadRandom)
import Data.Aeson ((.=))
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as BSL8
import Data.CaseInsensitive (CI)
import qualified Data.CaseInsensitive as CI
import Data.Function ((&))
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
import qualified TwitAnalysis.MiscWaiMiddleware as Misc
import qualified TwitAnalysis.OAuth.Signing as MySigning
import qualified TwitAnalysis.TwitterApiCallDemo as ApiCallDemo

parsePathComps :: T.Text -> [T.Text]
parsePathComps prefix = T.splitOn "/" prefix & filter (/= "")

joinPathComps :: [T.Text] -> T.Text
joinPathComps comps = T.intercalate "/" comps

stripPathPrefix :: [T.Text] -> T.Text -> T.Text
stripPathPrefix prefix =
  joinPathComps . addLeadingSlash . Misc.dropPrefix (==) prefix . parsePathComps
  where
    addLeadingSlash = ("" :)

withUtf8 :: (T.Text -> T.Text) -> BS.ByteString -> BS.ByteString
withUtf8 transform = TE.encodeUtf8 . transform . TE.decodeUtf8

stripRequestPathPrefix :: [T.Text] -> HC.Request -> HC.Request
stripRequestPathPrefix prefix hreq =
  hreq {HC.path = withUtf8 (stripPathPrefix prefix) (HC.path hreq)}

waiRequestToHttpClient ::
     (MonadThrow m, MonadIO m) => Wai.Request -> m HC.Request
waiRequestToHttpClient wreq = do
  hreq0 <-
    let toBeOverwritten = "https://api.twitter.com" :: BS.ByteString
     in HC.parseRequest $
        BS8.unpack
          (toBeOverwritten <> Wai.rawPathInfo wreq <> Wai.rawQueryString wreq)
  -- bodyLbs :: BSL.ByteString <- liftIO $ Wai.strictRequestBody wreq
  -- let body = HC.RequestBodyLBS bodyLbs
  let body = convertBody wreq
  let hreq1 =
        hreq0
          { HC.method = Wai.requestMethod wreq
          , HC.requestHeaders =
              HC.requestHeaders hreq0 ++
              (filter keepHeader (Wai.requestHeaders wreq))
          , HC.redirectCount = 0 -- Always pass redirects back to the client.
          , HC.requestBody = body
              -- HU.requestBodySourceChunkedIO (WU.sourceRequestBody wreq)
          -- , HC.decompress = const False
          }
  return hreq1
  where
    keepHeader (k, _) =
      k `notElem`
      [ "content-encoding"
      , "content-length"
      , "host"
      , "referer"
      , "origin"
      , "cookie"
      , "user-agent"
      , "dnt"
      ]

requestConversionLoggingMiddleware :: Wai.Middleware
requestConversionLoggingMiddleware next request respond = do
  hreq :: HC.Request <- waiRequestToHttpClient request
  putStrLn
    ("requestConversionLoggingMiddleware got this request: " ++ show hreq)
  next request respond

rebaseToTwitter :: HC.Request -> HC.Request
rebaseToTwitter hreq =
  hreq
    { HC.host = "api.twitter.com"
    , HC.path = "/1.1" <> HC.path hreq
    , HC.secure = True
    , HC.port = 443
    }

-- | Pick out pieces of the HC.Response and explain them to Scotty
interpretHttpClientResponse :: HC.Response BSL.ByteString -> Scotty.ActionM ()
interpretHttpClientResponse res = do
  Scotty.status (HC.responseStatus res)
  forM_ (HC.responseHeaders res) $ \(nameCiBs, valueBs) -> do
    Scotty.setHeader
      (TL.fromStrict (TE.decodeUtf8 (CI.original nameCiBs)))
      (TL.fromStrict (TE.decodeUtf8 valueBs))
  Scotty.raw (HC.responseBody res)

pickHeaders :: Eq k => [k] -> [(k, v)] -> [(k, v)]
pickHeaders keys orig = filter ((`elem` keys) . fst) orig

hideHeaders :: Eq k => [k] -> [(k, v)] -> [(k, v)]
hideHeaders keys orig = filter ((`notElem` keys) . fst) orig

-- | Picking out the Conduit part of `doUpstreamRequest` from `http-proxy`
convertBody :: Wai.Request -> HC.RequestBody
convertBody wreq =
  case Wai.requestBodyLength wreq of
    Wai.ChunkedBody -> HU.requestBodySourceChunkedIO (WU.sourceRequestBody wreq)
    Wai.KnownLength l ->
      HU.requestBodySourceIO (fromIntegral l) (WU.sourceRequestBody wreq)

callTwitterApi ::
     HC.Manager
  -> Cred Permanent
  -> OAuthParams.Server
  -> T.Text
  -> Wai.Request
  -> IO (HC.Response BSL.ByteString)
callTwitterApi httpMan accessCred srv passthruRoute wreq = do
  let passthruRouteComps = parsePathComps passthruRoute
      strippedRawPathInfoAndQuery =
        T.unpack $
        stripPathPrefix
          passthruRouteComps
          (TE.decodeUtf8 (Wai.rawPathInfo wreq <> Wai.rawQueryString wreq))
  let url =
        "https://api.twitter.com/1.1" <> strippedRawPathInfoAndQuery :: String
      method = Wai.requestMethod wreq :: BS.ByteString
      headers
        -- ^ Only a select few headers to keep
       =
        pickHeaders ["Content-Type", "Accept"] (Wai.requestHeaders wreq) :: [HHT.Header]
  putStrLn ("callTwitterApi about to use this URL: " ++ url)
  putStrLn ("callTwitterApi about to use this method: " ++ show method)
  putStrLn
    ("callTwitterApi about to use these request headers: " ++ show headers)
  let body = convertBody wreq
  hreq0 <- HC.parseRequest url
  let hreq1 =
        hreq0
          { HC.method = method
          , HC.requestHeaders = headers
          , HC.requestBody = body
          }
  putStrLn
    ("callTwitterApi about to sign this unauthenticated HC.Request: " ++
     show hreq1)
  signed <- MySigning.oauth accessCred srv hreq1
  putStrLn
    ("callTwitterApi about to fire off this signed HC.Request: " ++ show signed)
  putStrLn
    ("In particular, the request headers are: " ++
     show (HC.requestHeaders signed))
  ApiCallDemo.withEntireResponse signed httpMan return

-- Forget about MonadBaseControl; Scotty's `ActionM ()` doesn't even have
-- a MonadRandom instance.
withTwitterApiCall ::
     HC.Manager
  -> Cred Permanent
  -> OAuthParams.Server
  -> T.Text
  -> Wai.Request
  -> (HC.Response BSL.ByteString -> IO a)
  -> IO a
withTwitterApiCall httpMan accessCred srv passthruRoute wreq cont = do
  converted <- waiRequestToHttpClient wreq
  putStrLn
    ("withTwitterApiCall converted into this HC.Request: " ++ show converted)
  let stripped = stripRequestPathPrefix (parsePathComps passthruRoute) converted
      rebased = rebaseToTwitter stripped
  signed <- MySigning.oauth accessCred srv rebased
  putStrLn
    ("withTwitterApiCall produced this signed request, ready to go: " ++
     show signed)
  putStrLn
    ("In particular, the signed request in withTwitterApiCall has these request headers: " ++
     show (HC.requestHeaders signed))
  ApiCallDemo.withEntireResponse signed httpMan $ \resp -> do
    putStrLn ("withTwitterApiCall received this response: " ++ show resp)
    cont resp

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
      hresponse <-
        liftIO $ callTwitterApi httpMan accessCred srv passthruRoute wreq
      interpretHttpClientResponse hresponse

matchesPassthruRoute :: T.Text -> Wai.Request -> Bool
matchesPassthruRoute passthruRoute wreq =
  withUtf8
    (stripPathPrefix (parsePathComps passthruRoute))
    (Wai.rawPathInfo wreq) /=
  Wai.rawPathInfo wreq

asMiddleware ::
     HC.Manager
  -> Cred Permanent
  -> OAuthParams.Server
  -> T.Text
  -> Wai.Middleware
asMiddleware httpMan accessCred srv passthruRoute =
  Wai.ifRequest (matchesPassthruRoute passthruRoute) reroute
  where
    reroute ::
         Wai.Application
      -> Wai.Request
      -> (Wai.Response -> IO Wai.ResponseReceived)
      -> IO Wai.ResponseReceived
    reroute inner wreq wrespond =
      HP.doUpstreamRequestVia
        (transformRequest accessCred srv passthruRoute)
        httpMan
        wrespond
        wreq
    transformRequest ::
         (MonadIO m, MonadRandom m)
      => Cred Permanent
      -> OAuthParams.Server
      -> T.Text
      -> HC.Request
      -> m HC.Request
    transformRequest accessCred srv passthruRoute =
      MySigning.oauth accessCred srv .
      rebaseToTwitter . stripRequestPathPrefix (parsePathComps passthruRoute)
