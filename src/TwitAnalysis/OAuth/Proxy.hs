{-# LANGUAGE OverloadedStrings #-}

-- | Building blocks for defining my "OAuth proxy" idea:
-- receive unsigned requests, and sign them.
--
-- Composition of dependencies from:
--
--  - Network.HTTP.Proxy.Patched: doUpstreamRequestVia
--  - TwitAnalysis.OAuth.Signing: sign
module TwitAnalysis.OAuth.Proxy where

import qualified Control.Exception as X
import Control.Monad.IO.Class (MonadIO)
import Crypto.Random (MonadRandom)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.Function ((&))
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Network.HTTP.Client as HC
import qualified Network.HTTP.Proxy.Patched as HP
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

import qualified TwitAnalysis.MiscWaiMiddleware as Misc
import qualified TwitAnalysis.OAuth.Signing as Signing

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

passthruRoute :: [T.Text]
passthruRoute = ["twitter-passthru"]

rebaseToTwitter :: HC.Request -> HC.Request
rebaseToTwitter hreq =
  hreq
    { HC.host = "api.twitter.com"
    , HC.path = "/1.1" <> HC.path hreq
    , HC.secure = True
    , HC.port = 443
    }

transformRequest ::
     (MonadIO m, MonadRandom m)
  => Cred Permanent
  -> OAuthParams.Server
  -> HC.Request
  -> m HC.Request
transformRequest accessCred srv =
  Signing.sign accessCred srv .
  rebaseToTwitter . stripRequestPathPrefix passthruRoute

matchesPassthruRoute :: Wai.Request -> Bool
matchesPassthruRoute wreq =
  withUtf8 (stripPathPrefix passthruRoute) (Wai.rawPathInfo wreq) /=
  Wai.rawPathInfo wreq

asMiddleware ::
     HC.Manager
  -> Cred Permanent
  -> OAuthParams.Server
  -> String
  -> Wai.Middleware
asMiddleware httpMan accessCred srv basePath =
  Wai.ifRequest matchesPassthruRoute reroute
  where
    reroute ::
         Wai.Application
      -> Wai.Request
      -> (Wai.Response -> IO Wai.ResponseReceived)
      -> IO Wai.ResponseReceived
    reroute inner wreq wrespond =
      HP.doUpstreamRequestVia
        (transformRequest accessCred srv)
        httpMan
        wrespond
        wreq
