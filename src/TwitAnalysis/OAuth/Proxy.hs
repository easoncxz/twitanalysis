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
parsePathComps prefix = T.splitOn "/" prefix -- & filter (/= "")

joinPathComps :: [T.Text] -> T.Text
joinPathComps comps = T.intercalate "/" comps

stripPathPrefix :: [T.Text] -> T.Text -> T.Text
stripPathPrefix prefix =
  joinPathComps . Misc.dropPrefix (==) (prependSlash prefix) . parsePathComps
  where
    prependSlash = ("" :)

withUtf8 :: (T.Text -> T.Text) -> BS.ByteString -> BS.ByteString
withUtf8 transform = TE.encodeUtf8 . transform . TE.decodeUtf8

stripRequestPathPrefix :: [T.Text] -> HC.Request -> HC.Request
stripRequestPathPrefix prefix hreq =
  hreq {HC.path = withUtf8 (stripPathPrefix prefix) (HC.path hreq)}

handlePassthruEndpoint ::
     Cred Permanent -> OAuthParams.Server -> String -> Wai.Middleware
handlePassthruEndpoint accessCred srv basePath inner wreq wrespond = undefined
