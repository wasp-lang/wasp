{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module Wasp.AppSpec.App (App (..)) where

import Prelude hiding (head)
import qualified Data.Aeson as Aeson
import Data.Data (Data)
import Data.Maybe (catMaybes)
import GHC.Generics (Generic)
import Wasp.AppSpec.App.Auth (Auth)
import Wasp.AppSpec.App.Client (Client)
import Wasp.AppSpec.App.Db (Db)
import Wasp.AppSpec.App.EmailSender (EmailSender)
import Wasp.AppSpec.App.Server (Server)
import Wasp.AppSpec.App.Wasp (Wasp)
import Wasp.AppSpec.App.WebSocket (WebSocket)
import Wasp.AppSpec.Core.IsDecl (IsDecl)
import Wasp.AppSpec.JSON (maybeToField)

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
  deriving (Show, Eq, Data, Generic, Aeson.FromJSON)

instance IsDecl App

instance Aeson.ToJSON App where
  toJSON app =
    let requiredFields =
          [ "wasp" Aeson..= wasp app,
            "title" Aeson..= title app
          ]
        optionalFields =
          [ maybeToField "head" (head (app :: App)),
            maybeToField "auth" (auth app),
            maybeToField "server" (server app),
            maybeToField "client" (client app),
            maybeToField "db" (db app),
            maybeToField "emailSender" (emailSender app),
            maybeToField "webSocket" (webSocket app)
          ]
     in Aeson.object (requiredFields <> catMaybes optionalFields)
