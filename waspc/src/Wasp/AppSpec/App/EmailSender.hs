{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module Wasp.AppSpec.App.EmailSender
  ( EmailSender (..),
    EmailProvider (..),
    EmailFromField (..),
  )
where

import qualified Data.Aeson as Aeson
import Data.Data (Data)
import Data.Maybe (catMaybes)
import GHC.Generics (Generic)
import Wasp.AppSpec.JSON (maybeToField)

data EmailSender = EmailSender
  { provider :: EmailProvider,
    defaultFrom :: Maybe EmailFromField
  }
  deriving (Show, Eq, Data, Generic, Aeson.FromJSON)

instance Aeson.ToJSON EmailSender where
  toJSON emailSender =
    let requiredFields = ["provider" Aeson..= provider emailSender]
        optionalFields =
          [ maybeToField "defaultFrom" (defaultFrom emailSender)
          ]
     in Aeson.object (requiredFields <> catMaybes optionalFields)

data EmailProvider = SMTP | SendGrid | Mailgun | Dummy
  deriving (Show, Eq, Data, Generic, Aeson.FromJSON, Aeson.ToJSON)

data EmailFromField = EmailFromField
  { name :: Maybe String,
    email :: String
  }
  deriving (Show, Eq, Data, Generic, Aeson.FromJSON)

instance Aeson.ToJSON EmailFromField where
  toJSON emailFrom =
    let requiredFields = ["email" Aeson..= email emailFrom]
        optionalFields =
          [ maybeToField "name" (name emailFrom)
          ]
     in Aeson.object (requiredFields <> catMaybes optionalFields)
