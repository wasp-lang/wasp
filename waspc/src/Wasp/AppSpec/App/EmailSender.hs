{-# LANGUAGE DeriveDataTypeable #-}

module Wasp.AppSpec.App.EmailSender
  ( EmailSender (..),
    EmailProvider (..),
    EmailFromField (..),
  )
where

import Data.Data (Data)

data EmailSender = EmailSender
  { provider :: EmailProvider,
    defaultFrom :: Maybe EmailFromField
  }
  deriving (Show, Eq, Data)

data EmailProvider = SMTP | SendGrid | Mailgun
  deriving (Eq, Data, Show)

data EmailFromField = EmailFromField
  { name :: Maybe String,
    email :: String
  }
  deriving (Show, Eq, Data)
