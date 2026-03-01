{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module Wasp.AppSpec.App (App (..), ModuleProvide (..), ModuleEntityMap (..)) where

import qualified Data.Aeson as Aeson
import Data.Aeson (FromJSON (..), Value)
import Data.Data (Data)
import qualified Data.Map as Map
import GHC.Generics (Generic)
import Wasp.AppSpec.App.Auth (Auth)
import Wasp.AppSpec.App.Client (Client)
import Wasp.AppSpec.App.Db (Db)
import Wasp.AppSpec.App.EmailSender (EmailSender)
import Wasp.AppSpec.App.Server (Server)
import Wasp.AppSpec.App.Wasp (Wasp)
import Wasp.AppSpec.App.WebSocket (WebSocket)
import Wasp.AppSpec.Core.IsDecl (IsDecl)
import Wasp.AppSpec.ExtImport (ExtImport)

data ModuleProvide = ModuleProvide
  { packageName :: String,
    values :: Map.Map String Value
  }
  deriving (Show, Eq, Data, Generic, FromJSON)

data ModuleEntityMap = ModuleEntityMap
  { _memPackageName :: String,
    _memEntityMap :: Map.Map String String
  }
  deriving (Show, Eq, Data, Generic)

instance FromJSON ModuleEntityMap where
  parseJSON = Aeson.withObject "ModuleEntityMap" $ \v ->
    ModuleEntityMap
      <$> v Aeson..: "packageName"
      <*> v Aeson..: "entityMap"

data App = App
  { wasp :: Wasp,
    title :: String,
    head :: Maybe [String],
    auth :: Maybe Auth,
    server :: Maybe Server,
    client :: Maybe Client,
    db :: Maybe Db,
    emailSender :: Maybe EmailSender,
    webSocket :: Maybe WebSocket,
    moduleServerSetupFns :: Maybe [ExtImport],
    moduleClientSetupFns :: Maybe [ExtImport],
    moduleProvides :: Maybe [ModuleProvide],
    moduleEntityMaps :: Maybe [ModuleEntityMap]
  }
  deriving (Show, Eq, Data, Generic, FromJSON)

instance IsDecl App
