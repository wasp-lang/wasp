{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module Wasp.AppSpec.App.EmailSender
  ( EmailSender (..),
    EmailProvider (..),
    EmailFromField (..),
    isDummyEmailSender,
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

data EmailProvider = SMTP | SendGrid | Mailgun | Resend | Dummy
  deriving (Show, Eq, Data, Generic, FromJSON, ToJSON)

isDummyEmailSender :: EmailSender -> Bool
isDummyEmailSender emailSender = provider emailSender == Dummy

data EmailFromField = EmailFromField
  { name :: Maybe String,
    email :: String
  }
  deriving (Show, Eq, Data, Generic, FromJSON, ToJSON)
