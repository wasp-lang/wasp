{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Wasp.AppSpec.App.EmailSender
  ( EmailSender (..),
    EmailProvider (..),
    EmailFromField (..),
    DevMailboxConfig (..),
    MaxEmails (..),
    isDummyEmailSender,
  )
where

import Data.Aeson (FromJSON (parseJSON), ToJSON (toJSON), Value (Number, String))
import Data.Data (Data)
import GHC.Generics (Generic)

data EmailSender = EmailSender
  { provider :: EmailProvider,
    defaultFrom :: Maybe EmailFromField,
    -- | Only used by the Dummy provider.
    mailbox :: Maybe DevMailboxConfig
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

data DevMailboxConfig = DevMailboxConfig
  { maxEmails :: Maybe MaxEmails
  }
  deriving (Show, Eq, Data, Generic, FromJSON, ToJSON)

-- | The spec package encodes `Infinity` as "unlimited".
data MaxEmails = MaxEmailsLimit Int | MaxEmailsUnlimited
  deriving (Show, Eq, Data, Generic)

instance FromJSON MaxEmails where
  parseJSON (String "unlimited") = pure MaxEmailsUnlimited
  parseJSON value@(Number _) = do
    limit <- parseJSON value
    if limit >= 0
      then pure $ MaxEmailsLimit limit
      else fail "maxEmails must not be negative"
  parseJSON _ = fail "maxEmails must be an integer or \"unlimited\""

instance ToJSON MaxEmails where
  toJSON (MaxEmailsLimit limit) = toJSON limit
  toJSON MaxEmailsUnlimited = String "unlimited"
