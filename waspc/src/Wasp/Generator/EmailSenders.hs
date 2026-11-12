module Wasp.Generator.EmailSenders
  ( getEnabledEmailProvidersJson,
    isDevMailboxEnabled,
    devMailboxRoutePath,
    devMailboxDirInWaspProjectDir,
    getDevMailboxMaxEmails,
  )
where

import Data.Aeson (KeyValue ((.=)), object)
import qualified Data.Aeson as Aeson
import Data.Maybe (fromMaybe)
import StrongPath (Dir', Path', Rel, reldir, (</>))
import Wasp.AppSpec (AppSpec)
import qualified Wasp.AppSpec as AS
import qualified Wasp.AppSpec.App as AS.App
import qualified Wasp.AppSpec.App.EmailSender as AS.App.EmailSender
import Wasp.AppSpec.Valid (getApp)
import Wasp.Project.Common (WaspProjectDir, dotWaspDirInWaspProjectDir)

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
        ("isResendProviderEnabled", AS.App.EmailSender.Resend),
        ("isDummyProviderEnabled", AS.App.EmailSender.Dummy)
      ]
    makeProviderJson (key, name) = key .= (enabledEmailSenderName == name)
    enabledEmailSenderName = AS.App.EmailSender.provider emailSender

isDevMailboxEnabled :: AppSpec -> Bool
isDevMailboxEnabled spec =
  AS.isDevelopment spec
    && maybe False AS.App.EmailSender.isDummyEmailSender (AS.App.emailSender $ getApp spec)

devMailboxRoutePath :: String
devMailboxRoutePath = "/__wasp/mailbox"

devMailboxDirInWaspProjectDir :: Path' (Rel WaspProjectDir) Dir'
devMailboxDirInWaspProjectDir = dotWaspDirInWaspProjectDir </> [reldir|mailbox|]

getDevMailboxMaxEmails :: AS.App.EmailSender.EmailSender -> AS.App.EmailSender.MaxEmails
getDevMailboxMaxEmails emailSender =
  fromMaybe defaultMaxEmails $
    AS.App.EmailSender.mailbox emailSender >>= AS.App.EmailSender.maxEmails
  where
    defaultMaxEmails = AS.App.EmailSender.MaxEmailsLimit 500
