{-# LANGUAGE DeriveDataTypeable #-}

module Wasp.AppSpec.App.Email
  ( Email (..),
    EmailProvider (..),
  )
where

import Data.Data (Data)

data Email = Email
  { provider :: EmailProvider,
    sender :: Maybe Sender
  }
  deriving (Show, Eq, Data)

data EmailProvider = SMTP | SendGrid
  deriving (Eq, Data)

instance Show EmailProvider where
  show SMTP = "smtp"
  show SendGrid = "sendgrid"

data Sender = Sender
  { name :: String,
    email :: String
  }
  deriving (Show, Eq, Data)
