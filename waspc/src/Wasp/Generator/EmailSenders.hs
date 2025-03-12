module Wasp.Generator.EmailSenders
  ( getEnabledEmailProvidersJson,
  )
where

import Data.Aeson (KeyValue ((.=)), object)
import qualified Data.Aeson as Aeson
import qualified Wasp.AppSpec.App.EmailSender as AS.App.EmailSender

getEnabledEmailProvidersJson :: AS.App.EmailSender.EmailSender -> Aeson.Value
getEnabledEmailProvidersJson emailSender =
  object $
    makeProviderJson
      <$> providersKeyAndName
  where
    providersKeyAndName =
      [ ("isSmtpProviderEnabled", AS.App.EmailSender.SMTP),
        ("isSendGridProviderEnabled", AS.App.EmailSender.SendGrid),
        ("isMailgunProviderEnabled", AS.App.EmailSender.Mailgun),
        ("isDummyProviderEnabled", AS.App.EmailSender.Dummy)
      ]
    makeProviderJson (key, name) = key .= (enabledEmailSenderName == name)
    enabledEmailSenderName = AS.App.EmailSender.provider emailSender
