{-# LANGUAGE DeriveDataTypeable #-}

module Wasp.AppSpec.App.EmailSender
  ( EmailSender (..),
    EmailProvider (..),
    Sender (..),
  )
where

import Data.Data (Data)

data EmailSender = EmailSender
  { provider :: EmailProvider,
    sender :: Sender
  }
  deriving (Show, Eq, Data)

data EmailProvider = SMTP | SendGrid | Mailgun
  deriving (Eq, Data)

instance Show EmailProvider where
  show SMTP = "smtp"
  show SendGrid = "sendgrid"
  show Mailgun = "mailgun"

data Sender = Sender
  { title :: Maybe String,
    email :: String
  }
  deriving (Show, Eq, Data)
