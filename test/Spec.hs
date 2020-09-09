{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy.Char8 as BSL8
import qualified Data.Text as T
import qualified Network.HTTP.Client as HC
import Test.Hspec (Spec, describe, hspec, it, shouldBe)
import qualified Test.QuickCheck as QC
import qualified Web.Twitter.Types as Twitter

import qualified TwitAnalysis.MiscWaiMiddleware as Misc
import qualified TwitAnalysis.OAuth.Proxy as OP

oauthProxyTests :: Spec
oauthProxyTests =
  describe "TwitAnalysis.OAuth.Proxy" $ do
    describe "parsePathComps" $ do
      it "can parse a one-comp path" $
        OP.parsePathComps "/foo" `shouldBe` ["", "foo"]
      it "can parse a zero-comp path" $ OP.parsePathComps "" `shouldBe` [""]
      it "can parse a slash" $ OP.parsePathComps "/" `shouldBe` ["", ""]
      it "can parse a multi-comp path" $
        OP.parsePathComps "/foo/bar" `shouldBe` ["", "foo", "bar"]
      it "can parse a multi-comp path ending with a slash" $
        OP.parsePathComps "/foo/bar/" `shouldBe` ["", "foo", "bar", ""]
      it "has round-trip identity with joinPathComps" $
        QC.property $ \(s :: String) ->
          let t = T.pack s
           in OP.joinPathComps (OP.parsePathComps t) == t &&
              OP.parsePathComps (OP.joinPathComps (OP.parsePathComps t)) ==
              OP.parsePathComps t
    describe "interplay with MiscWaiMiddleware.dropPrefix" $ do
      it "can drop a prefix" $
        Misc.dropPrefix (==) ["foo"] ["foo", "bar"] `shouldBe` ["bar"]
      it "can cancel if needed" $
        Misc.dropPrefix (==) ["other"] ["foo", "bar"] `shouldBe` ["foo", "bar"]
    describe "stripPathPrefix" $ do
      it "returns an unmatching path" $ do
        let path = "/something-else" :: T.Text
        (OP.stripPathPrefix ["something"] path) `shouldBe` path
      it "returns an unmatching path with trailing slash" $ do
        let path = "/something-else/" :: T.Text
        (OP.stripPathPrefix ["something"] path) `shouldBe` path
      it "strips a matching prefix" $ do
        (OP.stripPathPrefix ["one"] "/one/two") `shouldBe` "/two"

main :: IO ()
main =
  hspec $ do
    oauthProxyTests
    describe "Twitter types" $ do
      it "can be de-serialised" $
        -- Sample taken from:
        --    https://developer.twitter.com/en/docs/twitter-api/v1/tweets/timelines/api-reference/get-statuses-user_timeline
        -- Fixes to sample needed: JSON-quoting of double-quotes in the `source` fields.
       do
        sampleJsonBs <- BSL8.readFile "test/resources/sample-user.json"
        case Aeson.eitherDecode sampleJsonBs of
          Left msg -> fail msg
          Right (status :: Twitter.Status) -> do
            Twitter.statusId status `shouldBe` 850007368138018817
            -- BSL8.putStrLn (Aeson.encode status)
