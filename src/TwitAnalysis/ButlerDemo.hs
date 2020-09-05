{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NamedFieldPuns #-}

module TwitAnalysis.ButlerDemo where

import Prelude hiding (div, head, id)

import Data.String (IsString)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Text.Blaze.Html.Renderer.Text as BlazeT
import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html5 (Markup, (!))
import qualified Text.Blaze.Html5.Attributes as A
import qualified Web.Scotty as Scotty
import qualified Web.Scotty.Session as Session

newtype MySessionType =
  MySessionType Text

data Env =
  UnsafeEnv
    { envSessionMan :: Session.ScottySM MySessionType
    }

newEnv :: IO Env
newEnv = do
  envSessionMan <- Session.createSessionManager
  return UnsafeEnv {envSessionMan}

nameFieldName :: IsString s => s
nameFieldName = "name-field"

recogniseGuestView :: Maybe MySessionType -> Markup
recogniseGuestView who =
  H.html $ do
    H.body $ do
      H.h1 "Welcome, my guest!"
      case who of
        Nothing -> do
          H.p "It must be your first time here!"
          H.p "please tell us something about yourself."
          H.form ! A.method "POST" ! A.action butlerPostPath $ do
            H.label ! A.for nameFieldName $ "Your name:"
            H.input ! A.name nameFieldName ! A.type_ "text"
            H.input ! A.type_ "submit" ! A.value "There you go"
        Just (MySessionType someone) -> do
          H.p . H.text $
            "Ah, you must be " <> someone <> "! I've met you last time."
          H.p "Welcome back!"
          H.form ! A.method "POST" ! A.action "/butler/goodbye" $ do
            H.button $ H.text "Please forget about me"

butlerGetPath :: IsString s => s
butlerGetPath = "/butler"

butlerPostPath :: IsString s => s
butlerPostPath = "/butler/register"

butlerPostPathLogout :: IsString s => s
butlerPostPathLogout = "/butler/goodbye"

recogniseGuest :: Session.ScottySM MySessionType -> Scotty.ActionM ()
recogniseGuest sessionMan = do
  session <- Session.readSession sessionMan
  Scotty.html . BlazeT.renderHtml $ recogniseGuestView session

registerGuest :: Session.ScottySM MySessionType -> Scotty.ActionM ()
registerGuest sessionMan = do
  name :: Text <- Scotty.param nameFieldName
  Session.modifySession sessionMan (const (Just (MySessionType name)))
  Scotty.redirect butlerGetPath

forgetGuest :: Session.ScottySM MySessionType -> Scotty.ActionM ()
forgetGuest sessionMan = do
  Session.modifySession sessionMan (const Nothing)
  Scotty.redirect butlerGetPath

registerButlerRoutes :: Env -> Scotty.ScottyM ()
registerButlerRoutes UnsafeEnv {envSessionMan = sessionMan} = do
  Scotty.get butlerGetPath (recogniseGuest sessionMan)
  Scotty.post butlerPostPath (registerGuest sessionMan)
  Scotty.post butlerPostPathLogout (forgetGuest sessionMan)
