module Wasp.Generator.ServerGenerator.EmailSender.Providers
  ( smtp,
    sendGrid,
    mailgun,
    EmailSenderProvider (..),
  )
where

import StrongPath (File', Path', Rel, relfile)
import qualified Wasp.AppSpec.App.Dependency as AS.Dependency
import qualified Wasp.Generator.ServerGenerator.Common as C
import qualified Wasp.SemanticVersion as SV

data EmailSenderProvider = EmailSenderProvider
  { npmDependency :: AS.Dependency.Dependency,
    setupFnPath :: Path' (Rel C.ServerTemplatesSrcDir) File',
    -- We have to use explicit boolean keys in templates (e.g. "isSMTPProviderEnabled") so each
    -- provider provides its own key which we pass to the template.
    isEnabledKey :: String
  }
  deriving (Show, Eq)

smtp :: EmailSenderProvider
smtp =
  EmailSenderProvider
    { npmDependency = nodeMailerDependency,
      setupFnPath = [relfile|email/core/providers/smtp.ts|],
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
    { npmDependency = sendGridDependency,
      setupFnPath = [relfile|email/core/providers/sendgrid.ts|],
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
    { npmDependency = mailgunDependency,
      setupFnPath = [relfile|email/core/providers/mailgun.ts|],
      isEnabledKey = "isMailgunProviderUsed"
    }
  where
    mailgunVersionRange :: SV.Range
    mailgunVersionRange = SV.Range [SV.backwardsCompatibleWith (SV.Version 0 5 1)]

    mailgunDependency :: AS.Dependency.Dependency
    mailgunDependency = AS.Dependency.make ("ts-mailgun", show mailgunVersionRange)