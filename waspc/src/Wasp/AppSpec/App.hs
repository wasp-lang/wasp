{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module Wasp.AppSpec.App (App (..), getDeploymentMode) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Data (Data)
import Data.List (intercalate)
import Data.Maybe (fromMaybe)
import GHC.Generics (Generic)
import Wasp.AppSpec.App.Auth (Auth, enabledAuthMethodNames)
import qualified Wasp.AppSpec.App.Auth as Auth
import Wasp.AppSpec.App.Client (Client)
import Wasp.AppSpec.App.Db (Db)
import Wasp.AppSpec.App.Deployment (Deployment)
import qualified Wasp.AppSpec.App.Deployment as Deployment
import Wasp.AppSpec.App.EmailSender (EmailSender)
import Wasp.AppSpec.App.Server (Server)
import Wasp.AppSpec.App.Wasp (Wasp)
import Wasp.AppSpec.App.WebSocket (WebSocket)
import Wasp.AppSpec.Core.IsDecl (IsDecl)
import Wasp.AppSpec.Core.Ref (refName)
import Wasp.Inspectable (Inspectable (..), InspectionEntry (InspectionEntry))

data App = App
  { wasp :: Wasp,
    title :: String,
    deployment :: Maybe Deployment,
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

getDeploymentMode :: App -> Deployment.DeploymentMode
getDeploymentMode app =
  fromMaybe Deployment.Split $ deployment app >>= Deployment.mode

instance Inspectable App where
  inspect app =
    [ InspectionEntry "App" $
        ("Title", title app)
          : inspectAuth' (auth app)
    ]
    where
      inspectAuth' Nothing = []
      inspectAuth' (Just appAuth) =
        [ ("Auth", intercalate ", " $ enabledAuthMethodNames $ Auth.methods appAuth),
          ("User entity", refName (Auth.userEntity appAuth))
        ]
