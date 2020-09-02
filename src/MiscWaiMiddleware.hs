{-# LANGUAGE OverloadedStrings #-}

module MiscWaiMiddleware where

import qualified Data.ByteString.Char8 as BS8
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Tuple (uncurry)
import qualified Network.HTTP.Types as Http
import qualified Network.Wai as Wai
import qualified Network.Wai.Middleware.Rewrite as Rewrite
import qualified Network.Wai.Middleware.Static as Static
import qualified Text.ParserCombinators.ReadP as ReadP

favicon :: Wai.Response
favicon =
  Wai.responseFile
    Http.status200
    [("Content-Type", "image/x-icon")]
    "frontend-app/static/favicon.ico"
    Nothing

justFavicon :: Wai.Middleware
justFavicon next request respond =
  case Wai.rawPathInfo request of
    "/favicon.ico" -> respond favicon
    _ -> next request respond

myStaticMiddleware :: Wai.Middleware
myStaticMiddleware =
  Static.staticPolicy (Static.noDots <> Static.addBase "frontend-app/static/")

dropPrefix :: (a -> b -> Bool) -> [a] -> [b] -> [b]
dropPrefix eq (x:xs) (y:ys) =
  if x `eq` y
    then dropPrefix eq xs ys
    else ys
dropPrefix eq _ ys = ys

eatRequestPathPrefix :: [Text] -> Wai.Request -> Wai.Request
eatRequestPathPrefix prefix =
  Rewrite.rewriteRequestPure $ \(pathComps, query) _headers ->
    (dropPrefix (==) prefix pathComps, query)

parsePrefix :: String -> [Text]
parsePrefix prefix = filter (/= "") (Text.splitOn "/" (Text.pack prefix))

prefixedStatic :: String -> Wai.Middleware
prefixedStatic prefix next request respond = do
  let rawPathInfoStr = BS8.unpack (Wai.rawPathInfo request)
  putStrLn ("The rawPathInfo is: " ++ rawPathInfoStr)
  case ReadP.readP_to_S (ReadP.string prefix) rawPathInfoStr of
    [(_thePrefix, rest)] -> do
      putStrLn ("The rest of the rawPathInfo is: " ++ rest)
      myStaticMiddleware
        next
        (eatRequestPathPrefix (parsePrefix rest) request)
        respond
    _ -> next request respond
