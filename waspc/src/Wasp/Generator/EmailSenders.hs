module Wasp.Generator.EmailSenders
  ( getEnabledEmailProvidersJson,
    isDevMailboxEnabled,
    devMailboxRoutePath,
    devMailboxDirInWaspProjectDir,
    getDevMailboxMaxMessages,
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

-- | The dev mailbox keeps emails sent with the Dummy email sender, which is
-- only allowed in development.
isDevMailboxEnabled :: AppSpec -> Bool
isDevMailboxEnabled spec =
  AS.isDevelopment spec
    && maybe False AS.App.EmailSender.isDummyEmailSender (AS.App.emailSender $ getApp spec)

devMailboxRoutePath :: String
devMailboxRoutePath = "/__wasp/mailbox"

devMailboxDirInWaspProjectDir :: Path' (Rel WaspProjectDir) Dir'
devMailboxDirInWaspProjectDir = dotWaspDirInWaspProjectDir </> [reldir|mailbox|]

getDevMailboxMaxMessages :: AS.App.EmailSender.EmailSender -> AS.App.EmailSender.MaxMessages
getDevMailboxMaxMessages emailSender =
  fromMaybe defaultMaxMessages $
    AS.App.EmailSender.mailbox emailSender >>= AS.App.EmailSender.maxMessages
  where
    defaultMaxMessages = AS.App.EmailSender.MaxMessagesLimit 500
