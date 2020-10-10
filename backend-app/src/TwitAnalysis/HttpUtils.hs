{-# LANGUAGE ScopedTypeVariables #-}

module TwitAnalysis.HttpUtils where

import qualified Data.Aeson as A
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Network.HTTP.Client as HC

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

withJsonResponse ::
     A.FromJSON j => HC.Request -> HC.Manager -> (HC.Response j -> IO a) -> IO a
withJsonResponse req man cont =
  withEntireResponse req man $ \resp -> do
    let bsl = HC.responseBody resp :: BSL.ByteString
    let ei = A.eitherDecode bsl
    case ei of
      Left e -> fail $ "Cannot decode JSON: " ++ e
      Right j -> cont (const j <$> resp)

withResponseLbs :: HC.Request -> HC.Manager -> (BSL.ByteString -> IO a) -> IO a
withResponseLbs req man cont =
  withEntireResponse req man (cont . HC.responseBody)
