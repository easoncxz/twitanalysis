{-# LANGUAGE OverloadedStrings #-}

module Lib
  ( someFunc
  , scottyDefinition
  ) where

import qualified Web.Scotty as Scotty

scottyDefinition :: Scotty.ScottyM ()
scottyDefinition = do
  Scotty.get "/" $ do Scotty.html "<h1>Hello from Scotty!</h1>"

someFunc :: IO ()
someFunc = putStrLn "someFunc"
