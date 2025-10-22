{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module Wasp.AppSpec.App.EmailSender
  ( EmailSender (..),
    EmailProvider (..),
    EmailFromField (..),
  )
where

import Data.Aeson (FromJSON, ToJSON)
import Data.Data (Data)
import GHC.Generics (Generic)

data EmailSender = EmailSender
  { provider :: EmailProvider,
    defaultFrom :: Maybe EmailFromField
  }
  deriving (Show, Eq, Data, Generic, FromJSON, ToJSON)

data EmailProvider = SMTP | SendGrid | Mailgun | Dummy
  deriving (Show, Eq, Data, Generic, FromJSON, ToJSON)

data EmailFromField = EmailFromField
  { name :: Maybe String,
    email :: String
  }
  deriving (Show, Eq, Data, Generic, FromJSON, ToJSON)
