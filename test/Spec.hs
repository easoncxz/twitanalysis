{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy.Char8 as BSL8
import qualified Data.Maybe as Maybe
import qualified Data.Text as T
import qualified Network.HTTP.Client as HC
import Test.Hspec (Spec, describe, hspec, it, shouldBe, xit)
import qualified Test.QuickCheck as QC
import qualified Web.Twitter.Types as Twitter

import qualified TwitAnalysis.MiscWaiMiddleware as Misc
import qualified TwitAnalysis.OAuth.Proxy as OP

oauthProxyTests :: Spec
oauthProxyTests =
  describe "TwitAnalysis.OAuth.Proxy" $ do
    describe "parsePathComps" $ do
      it "can parse a one-comp path" $
        OP.parsePathComps "/foo" `shouldBe` ["foo"]
      it "can parse a zero-comp path" $ OP.parsePathComps "" `shouldBe` []
      it "can parse a slash" $ OP.parsePathComps "/" `shouldBe` []
      it "can parse a multi-comp path" $
        OP.parsePathComps "/foo/bar" `shouldBe` ["foo", "bar"]
      it "can parse a multi-comp path ending with a slash" $
        OP.parsePathComps "/foo/bar/" `shouldBe` ["foo", "bar"]
      describe
        "round-trip identity between parsePathComps and joinPathComps (up to slash-prefix and suffix)" $ do
        let normalise orig =
              let withoutLeading =
                    Maybe.fromMaybe orig . T.stripPrefix "/" $ orig
                  alsoWithoutTrailing =
                    Maybe.fromMaybe withoutLeading . T.stripSuffix "/" $
                    withoutLeading
               in alsoWithoutTrailing
        it "has the right QuickCheck filter" $
          QC.property $ \(s :: String) ->
            let t = normalise (T.pack s)
             in if T.null t
                  then True
                  else T.index t 0 /= '/' && T.index t (T.length t - 1) /= '/'
        it "has round-trip identity with joinPathComps " $
          QC.property $ \(s :: String) ->
            let t = normalise (T.pack s)
             in OP.joinPathComps (OP.parsePathComps t) == t &&
                OP.parsePathComps (OP.joinPathComps (OP.parsePathComps t)) ==
                OP.parsePathComps t
    describe "interplay with MiscWaiMiddleware.dropPrefix" $ do
      it "can drop a prefix" $
        Misc.dropPrefix (==) ["foo"] ["foo", "bar"] `shouldBe` ["bar"]
      it "can cancel if no match" $
        Misc.dropPrefix (==) ["other"] ["foo", "bar"] `shouldBe` ["foo", "bar"]
    describe "stripPathPrefix" $ do
      it "returns an unmatching path" $ do
        let path = "/something-else" :: T.Text
        OP.stripPathPrefix ["something"] path `shouldBe` path
      it "loses the trailing slash (!!)" $ do
        OP.stripPathPrefix ["something"] "/something-slash/" `shouldBe`
          "/something-slash"
      it "strips a matching prefix" $ do
        (OP.stripPathPrefix ["one"] "/one/two") `shouldBe` "/two"
    describe "stripRequestPathPrefix" $ do
      it "returns an irrelevant request untouched" $ do
        let hreq = "http://google.com/one/two" :: HC.Request
        HC.path (OP.stripRequestPathPrefix ["other"] hreq) `shouldBe`
          HC.path hreq
      it "strips prefix for a matching request" $ do
        let hreq = "http://google.com/one/two" :: HC.Request
        HC.path (OP.stripRequestPathPrefix ["one"] hreq) `shouldBe`
          HC.path (hreq {HC.path = "/two"})
      it "unfortunately also strips trailing slashes" $ do
        let hreq = "http://google.com/one/two/" :: HC.Request
        HC.path (OP.stripRequestPathPrefix ["one"] hreq) `shouldBe`
          HC.path (hreq {HC.path = "/two"})

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
        sampleJsonBs <- BSL8.readFile "test/resources/sample-status.json"
        case Aeson.eitherDecode sampleJsonBs of
          Left msg -> fail msg
          Right (status :: Twitter.Status) -> do
            Twitter.statusId status `shouldBe` 850007368138018817
            -- BSL8.putStrLn (Aeson.encode status)
