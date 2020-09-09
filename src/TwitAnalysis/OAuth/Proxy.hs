{-# LANGUAGE OverloadedStrings #-}

-- | Building blocks for defining my "OAuth proxy" idea:
-- receive unsigned requests, and sign them.
module TwitAnalysis.OAuth.Proxy where

import qualified Blaze.ByteString.Builder as BlazeB
import Control.Concurrent.Async (race_)
import qualified Control.Exception as X
import Control.Monad.IO.Class (MonadIO)
import Crypto.Random (MonadRandom)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.CaseInsensitive as CI
import qualified Data.Conduit as U
import Data.Conduit ((.|))
import qualified Data.Conduit.Network as UN
import Data.Void (Void)
import qualified Network.HTTP.Client as HC
import qualified Network.HTTP.Client.Conduit as HCU
import qualified Network.HTTP.Conduit as HU
import qualified Network.HTTP.Types as HT
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
import qualified Network.Wai as Wai
import qualified Network.Wai.Conduit as WU

-- | Copy-pasted from `Network.HTTP.Proxy` from `http-proxy`
--
--    - https://hackage.haskell.org/package/http-proxy-0.1.1.0/docs/src/Network.HTTP.Proxy.html#local-6989586621679075573
--
-- Tweaked to add in a param for performing a request transform.
doUpstreamRequest ::
     HC.Manager
  -> (Wai.Response -> IO Wai.ResponseReceived)
  -> Wai.Request
  -> IO Wai.ResponseReceived
doUpstreamRequest mgr respond mwreq
  | Wai.requestMethod mwreq == "CONNECT" =
    respond $
    WU.responseRawSource
      (handleConnect mwreq)
      (Wai.responseLBS
         HT.status500
         [("Content-Type", "text/plain")]
         "No support for responseRaw")
  | otherwise = do
    hreq0 <-
      HC.parseRequest $
      BS8.unpack (Wai.rawPathInfo mwreq <> Wai.rawQueryString mwreq)
    let hreq =
          hreq0
            { HC.method = Wai.requestMethod mwreq
            , HC.requestHeaders =
                filter dropRequestHeader $ Wai.requestHeaders mwreq
            , HC.redirectCount = 0 -- Always pass redirects back to the client.
            , HC.requestBody =
                case Wai.requestBodyLength mwreq of
                  Wai.ChunkedBody ->
                    HU.requestBodySourceChunkedIO (WU.sourceRequestBody mwreq)
                  Wai.KnownLength l ->
                    HU.requestBodySourceIO
                      (fromIntegral l)
                      (WU.sourceRequestBody mwreq)
                -- Do not touch response body. Otherwise there may be discrepancy
                -- between response headers and the response content.
            , HC.decompress = const False
            }
    HC.withResponse hreq mgr $ \res -> do
      let body =
            U.mapOutput (U.Chunk . BlazeB.fromByteString) . HCU.bodyReaderSource $
            HC.responseBody res
          headers =
            (CI.mk "X-Via-Proxy", "yes") :
            filter dropResponseHeader (HC.responseHeaders res)
      respond $ WU.responseSource (HC.responseStatus res) headers body
  where
    dropRequestHeader (k, _) =
      k `notElem` ["content-encoding", "content-length"]
    dropResponseHeader (k, _) = k `notElem` []

-- handleConnect :: Wai.Request -> ConduitT IO BS.ByteString -> ConduitT BS.ByteString IO () -> IO ()
handleConnect ::
     Wai.Request
  -> U.ConduitT () BS.ByteString IO ()
  -> U.ConduitT BS.ByteString Void IO a
  -> IO ()
handleConnect wreq fromClient toClient = do
  let (host, port) =
        case BS8.break (== ':') $ Wai.rawPathInfo wreq of
          (x, "") -> (x, 80)
          (x, y) ->
            case BS8.readInt $ BS.drop 1 y of
              Just (port', _) -> (x, port')
              Nothing -> (x, 80)
      settings = UN.clientSettings port host
  UN.runTCPClient settings $ \ad -> do
    _ <- U.runConduit $ U.yield "HTTP/1.1 200 OK\r\n\r\n" .| toClient
    race_
      (U.runConduit $ fromClient .| UN.appSink ad)
      (U.runConduit $ UN.appSource ad .| toClient)
