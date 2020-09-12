{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}

-- | Copyright 2014 Alexander Thiemann
--
-- This module is copy-pasted and then adapted from:
--  https://github.com/agrafix/scotty-session/blob/master/Web/Scotty/Session.hs
--
-- Dependent packages:
--      - base64-bytestring
--      - crypto-api
--      - time
--      - unordered-containers
--
module Web.Scotty.Session
  ( createSessionManager
  , modifySession
  , readSession
  , ScottySM
  ) where

import Control.Arrow
import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Monoid

import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Data.Text.Encoding as TEnc
import qualified Data.Text.Lazy as TL

-- | crypto-api modules:
-- import Crypto.Random (SystemRandom, genBytes, newGenIO)
-- import Crypto.Types (ByteLength)
--
-- replace with cryptonite functions that look equivalent:
import Crypto.Random.Types (getRandomBytes)

import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as B64

import Network.Wai

import Data.Time.Clock
import Data.Time.Format

import Web.Scotty.Trans

data Session a =
  Session
    { sess_id :: T.Text
    , sess_validUntil :: UTCTime
    , sess_content :: Maybe a
    }
  deriving (Show, Eq)

type SessionJar a = TVar (HM.HashMap T.Text (Session a))

newtype ScottySM a =
  ScottySM
    { _unSessionManager :: SessionJar a
    }

-- | Create a new session manager
createSessionManager :: IO (ScottySM a)
createSessionManager = do
  storage <- atomically $ newTVar HM.empty
  forkIO $ maintainSessions storage
  return $ ScottySM storage

-- | Modify the current users session
modifySession ::
     (MonadIO m, ScottyError e)
  => ScottySM a
  -> (Maybe a -> Maybe a)
  -> ActionT e m ()
modifySession sm@(ScottySM storage) fun = do
  oldS <- readSession' sm id
  let newS = oldS {sess_content = fun (sess_content oldS)}
  liftIO $ insertSession newS storage

-- | Read the current users session
readSession :: (MonadIO m, ScottyError e) => ScottySM a -> ActionT e m (Maybe a)
readSession sm = readSession' sm sess_content

readSession' ::
     (MonadIO m, ScottyError e)
  => ScottySM a
  -> (Session a -> b)
  -> ActionT e m b
readSession' (ScottySM storage) fun = do
  mSession <- loadSession storage
  case mSession of
    Just s -> return $ fun s
    Nothing -> do
      newS <- liftIO createSession
      liftIO $ insertSession newS storage
      setCookie newS
      return $ fun newS

sessionTTL :: NominalDiffTime
sessionTTL = 36000 -- in seconds

-- Typed as the Int-alias `ByteLength` if using crypto-api modules
sessionIdEntropy :: Int
sessionIdEntropy = 12

createSession :: IO (Session a)
createSession = do
  sid <- TEnc.decodeUtf8 . B64.encode <$> getRandomBytes sessionIdEntropy
  now <- getCurrentTime
  let validUntil = addUTCTime sessionTTL now
  return $ Session sid validUntil Nothing

insertSession :: Session a -> SessionJar a -> IO ()
insertSession sess sessions =
  atomically $ modifyTVar sessions $ \m -> HM.insert (sess_id sess) sess m

getSession :: T.Text -> SessionJar a -> IO (Maybe (Session a))
getSession sessId sessions = do
  s <- atomically $ readTVar sessions
  return $ HM.lookup sessId s

maintainSessions :: SessionJar a -> IO ()
maintainSessions sessions = do
  now <- getCurrentTime
  let stillValid sess = sess_validUntil sess > now
  atomically $ modifyTVar sessions $ \m -> HM.filter stillValid m
  threadDelay 1000000
  maintainSessions sessions

-- | easoncxz's changes: mark cookie as HTTP-only
--
-- To be marked also `Secure` when I get some local development setup going with TLS
--
-- Advice taken from:
--    - https://www.freecodecamp.org/news/session-hijacking-and-how-to-stop-it-711e3683d1ac/
setCookie :: (Monad m, ScottyError e) => Session a -> ActionT e m ()
setCookie sess = do
  let formattedExp =
        TL.pack $
        formatTime defaultTimeLocale "%a, %d %b %Y %X %Z" (sess_validUntil sess)
  setHeader "Set-Cookie" $
    "sid=" <>
    TL.fromStrict (sess_id sess) <>
    "; Path=/; Expires=" <> formattedExp <> "; HttpOnly"

loadSession ::
     (MonadIO m, ScottyError e)
  => SessionJar a
  -> ActionT e m (Maybe (Session a))
loadSession sessions = do
  req <- request
  liftIO $ getUserSession req sessions

getUserSession :: Request -> SessionJar a -> IO (Maybe (Session a))
getUserSession req sessions =
  case lookup "cookie" (requestHeaders req) >>=
       lookup "sid" . parseCookies . TEnc.decodeUtf8 of
    Just sid -> lookupSession sid
    Nothing -> return Nothing
  where
    lookupSession sid = getSession sid sessions

parseCookies :: T.Text -> [(T.Text, T.Text)]
parseCookies = map parseCookie . T.splitOn ";" . T.concat . T.words
  where
    parseCookie = first T.init . T.breakOnEnd "="
