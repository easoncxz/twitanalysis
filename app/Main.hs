module Main where

import qualified Web.Scotty as Scotty

import qualified Lib

main :: IO ()
main = Scotty.scotty 5000 Lib.scottyDefinition
