{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module Wasp.AppSpec.App (App (..)) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Data (Data)
import Data.List (intercalate)
import GHC.Generics (Generic)
import Wasp.AppSpec.App.Auth (Auth, enabledAuthMethodNames)
import qualified Wasp.AppSpec.App.Auth as Auth
import Wasp.AppSpec.App.Client (Client)
import Wasp.AppSpec.App.Db (Db)
import Wasp.AppSpec.App.EmailSender (EmailSender)
import Wasp.AppSpec.App.Server (Server)
import Wasp.AppSpec.App.Wasp (Wasp)
import Wasp.AppSpec.App.WebSocket (WebSocket)
import Wasp.AppSpec.Core.Inspectable (Inspectable (..), InspectionEntry (..))
import Wasp.AppSpec.Core.IsDecl (IsDecl)
import Wasp.AppSpec.Core.Ref (refName)

data App = App
  { wasp :: Wasp,
    title :: String,
    head :: Maybe [String],
    auth :: Maybe Auth,
    server :: Maybe Server,
    client :: Maybe Client,
    db :: Maybe Db,
    emailSender :: Maybe EmailSender,
    webSocket :: Maybe WebSocket
  }
  deriving (Show, Eq, Data, Generic, FromJSON, ToJSON)

instance IsDecl App

instance Inspectable App where
  inspectionSection = "App"
  inspect (name, app) =
    InspectionEntry
      [ name,
        show $ title app,
        maybe "" showAuth (auth app)
      ]
    where
      showAuth appAuth =
        "auth: "
          ++ intercalate ", " (enabledAuthMethodNames $ Auth.methods appAuth)
          ++ " (user entity: "
          ++ refName (Auth.userEntity appAuth)
          ++ ")"
