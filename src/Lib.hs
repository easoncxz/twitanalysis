{-# LANGUAGE OverloadedStrings #-}

module Lib
  ( scottyDefinition
  ) where

import qualified Web.Scotty as Scotty

scottyDefinition :: Scotty.ScottyM ()
scottyDefinition = do
  Scotty.get "/" $ do Scotty.html "<h1>Hello from Scotty!</h1>"
