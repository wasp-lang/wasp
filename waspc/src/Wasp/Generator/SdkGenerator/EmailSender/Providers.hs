module Wasp.Generator.SdkGenerator.EmailSender.Providers
  ( smtp,
    sendGrid,
    mailgun,
    dummy,
    providersDirInSdkTemplatesDir,
    EmailSenderProvider (..),
  )
where

import StrongPath (Dir, File', Path', Rel, reldir, relfile)
import qualified Wasp.AppSpec.App.Dependency as AS.Dependency
import qualified Wasp.Generator.SdkGenerator.Common as C
import qualified Wasp.SemanticVersion as SV

data EmailSenderProvider = EmailSenderProvider
  { npmDependency :: Maybe AS.Dependency.Dependency,
    setupFnFile :: Path' (Rel ProvidersDir) File',
    -- We have to use explicit boolean keys in templates (e.g. "isSMTPProviderEnabled") so each
    -- provider provides its own key which we pass to the template.
    isEnabledKey :: String
  }
  deriving (Show, Eq)

data ProvidersDir

providersDirInSdkTemplatesDir :: Path' (Rel C.SdkTemplatesDir) (Dir ProvidersDir)
providersDirInSdkTemplatesDir = [reldir|server/email/core/providers|]

smtp :: EmailSenderProvider
smtp =
  EmailSenderProvider
    { npmDependency = Just nodeMailerDependency,
      setupFnFile = [relfile|smtp.ts|],
      isEnabledKey = "isSmtpProviderUsed"
    }
  where
    nodeMailerVersionRange :: SV.Range
    nodeMailerVersionRange = SV.Range [SV.backwardsCompatibleWith (SV.Version 6 9 1)]

    nodeMailerDependency :: AS.Dependency.Dependency
    nodeMailerDependency = AS.Dependency.make ("nodemailer", show nodeMailerVersionRange)

sendGrid :: EmailSenderProvider
sendGrid =
  EmailSenderProvider
    { npmDependency = Just sendGridDependency,
      setupFnFile = [relfile|sendgrid.ts|],
      isEnabledKey = "isSendGridProviderUsed"
    }
  where
    sendGridVersionRange :: SV.Range
    sendGridVersionRange = SV.Range [SV.backwardsCompatibleWith (SV.Version 7 7 0)]

    sendGridDependency :: AS.Dependency.Dependency
    sendGridDependency = AS.Dependency.make ("@sendgrid/mail", show sendGridVersionRange)

mailgun :: EmailSenderProvider
mailgun =
  EmailSenderProvider
    { npmDependency = Just mailgunDependency,
      setupFnFile = [relfile|mailgun.ts|],
      isEnabledKey = "isMailgunProviderUsed"
    }
  where
    mailgunVersionRange :: SV.Range
    mailgunVersionRange = SV.Range [SV.backwardsCompatibleWith (SV.Version 0 5 1)]

    mailgunDependency :: AS.Dependency.Dependency
    mailgunDependency = AS.Dependency.make ("ts-mailgun", show mailgunVersionRange)

dummy :: EmailSenderProvider
dummy =
  EmailSenderProvider
    { npmDependency = Nothing,
      setupFnFile = [relfile|dummy.ts|],
      isEnabledKey = "isDummyProviderUsed"
    }
