{-# LANGUAGE ScopedTypeVariables #-}

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy.Char8 as BSL8
import Test.Hspec (describe, hspec, it, shouldBe)
import qualified Web.Twitter.Types as Twitter

main :: IO ()
main =
  hspec $ do
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
