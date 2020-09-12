{-# LANGUAGE OverloadedStrings #-}

module TwitAnalysis.OAuth.Proxy.Internals
  ( parsePathComps
  , joinPathComps
  , stripPathPrefix
  , stripRequestPathPrefix
  , withUtf8
  ) where

import qualified Data.ByteString as BS
import Data.Function ((&))
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Network.HTTP.Client as HC

import qualified TwitAnalysis.MiscWaiMiddleware as Misc

parsePathComps :: T.Text -> [T.Text]
parsePathComps prefix = T.splitOn "/" prefix & filter (/= "")

joinPathComps :: [T.Text] -> T.Text
joinPathComps comps = T.intercalate "/" comps

withUtf8 :: (T.Text -> T.Text) -> BS.ByteString -> BS.ByteString
withUtf8 transform = TE.encodeUtf8 . transform . TE.decodeUtf8

stripPathPrefix :: [T.Text] -> T.Text -> T.Text
stripPathPrefix prefix =
  joinPathComps . addLeadingSlash . Misc.dropPrefix (==) prefix . parsePathComps
  where
    addLeadingSlash = ("" :)

stripRequestPathPrefix :: [T.Text] -> HC.Request -> HC.Request
stripRequestPathPrefix prefix hreq =
  hreq {HC.path = withUtf8 (stripPathPrefix prefix) (HC.path hreq)}
