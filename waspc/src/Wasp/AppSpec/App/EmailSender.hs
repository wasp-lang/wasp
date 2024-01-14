{-# LANGUAGE DeriveDataTypeable #-}

module Wasp.AppSpec.App.EmailSender
  ( EmailSender (..),
    EmailProvider (..),
    EmailFromField (..),
    defaultDummyEmailSender,
  )
where

import Data.Data (Data)

data EmailSender = EmailSender
  { provider :: EmailProvider,
    defaultFrom :: Maybe EmailFromField
  }
  deriving (Show, Eq, Data)

data EmailProvider = SMTP | SendGrid | Mailgun | Dummy
  deriving (Eq, Data, Show)

data EmailFromField = EmailFromField
  { name :: Maybe String,
    email :: String
  }
  deriving (Show, Eq, Data)

defaultDummyEmailSender :: EmailSender
defaultDummyEmailSender =
  EmailSender
    { provider = Dummy,
      defaultFrom =
        Just
          EmailFromField
            { email = "dummy@wasp-lang.dev",
              name = Just "Wasp Dummy Email Sender"
            }
    }
