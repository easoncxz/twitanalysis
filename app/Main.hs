{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString.Char8 as BS8
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Tuple (uncurry)
import qualified Debug.Trace as Trace
import qualified Network.Wai as Wai
import Network.Wai.Middleware.RequestLogger (logStdout)
import qualified Network.Wai.Middleware.Rewrite as Rewrite
import qualified Network.Wai.Middleware.Static as Static
import qualified Text.ParserCombinators.ReadP as ReadP
import qualified Web.Scotty as Scotty

import qualified Lib

scottyApp :: Lib.AppEnv -> Scotty.ScottyM ()
scottyApp appEnv = do
  Scotty.get "/" Lib.viewHomepage
  Scotty.get "/login" (Lib.startOAuthFlow appEnv)
  Scotty.get Lib.oauthCallbackPath (Lib.handleOAuthCallback appEnv)

myStaticMiddleware :: Wai.Middleware
myStaticMiddleware =
  Static.staticPolicy
    (trace <> Static.noDots <> Static.addBase "frontend-app/static/")
  where
    trace =
      Static.predicate
        (\s -> Trace.trace ("The static path is: " ++ show s) True)

eatPrefix :: [Text] -> Wai.Request -> Wai.Request
eatPrefix prefix =
  Rewrite.rewriteRequestPure $ \(pathComps, query) _headers ->
    let restComps = map snd . filter (uncurry (==)) $ zip prefix pathComps
     in (restComps, query)

parsePrefix :: String -> [Text]
parsePrefix prefix = filter (/= "") (Text.splitOn "/" (Text.pack prefix))

prefixedStatic :: String -> Wai.Middleware
prefixedStatic prefix next request respond = do
  putStrLn ("The rawPathInfo is: " ++ BS8.unpack (Wai.rawPathInfo request))
  case ReadP.readP_to_S
         (ReadP.string prefix)
         (BS8.unpack (Wai.rawPathInfo request)) of
    [(_thePrefix, rest)] -> do
      putStrLn ("The rest of the rawPathInfo is: " ++ rest)
      myStaticMiddleware next (eatPrefix (parsePrefix rest) request) respond
    _ -> next request respond

main :: IO ()
main = do
  appEnv <- Lib.newAppEnv
  putStrLn
    ("Using client credentials: " ++ show (Lib.appEnvOAuthClientCred appEnv))
  Scotty.scotty 5000 $ do
    Scotty.middleware logStdout
    Scotty.middleware (prefixedStatic "/static")
    scottyApp appEnv
