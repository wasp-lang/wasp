{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Wasp.AppSpec.App.EmailSender
  ( EmailSender (..),
    EmailProvider (..),
    EmailFromField (..),
    DevMailboxConfig (..),
    MaxMessages (..),
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
  { maxMessages :: Maybe MaxMessages
  }
  deriving (Show, Eq, Data, Generic, FromJSON, ToJSON)

-- | The spec package encodes `Infinity` as "unlimited".
data MaxMessages = MaxMessagesLimit Int | MaxMessagesUnlimited
  deriving (Show, Eq, Data, Generic)

instance FromJSON MaxMessages where
  parseJSON (String "unlimited") = pure MaxMessagesUnlimited
  parseJSON value@(Number _) = do
    limit <- parseJSON value
    if limit >= 0
      then pure $ MaxMessagesLimit limit
      else fail "maxMessages must not be negative"
  parseJSON _ = fail "maxMessages must be an integer or \"unlimited\""

instance ToJSON MaxMessages where
  toJSON (MaxMessagesLimit limit) = toJSON limit
  toJSON MaxMessagesUnlimited = String "unlimited"
