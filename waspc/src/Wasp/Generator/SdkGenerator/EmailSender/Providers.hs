module Wasp.Generator.SdkGenerator.EmailSender.Providers
  ( smtp,
    sendGrid,
    mailgun,
    resend,
    dummy,
    EmailSenderProvider (..),
    getEmailSenderProvider,
  )
where

import StrongPath (File', Path', Rel, relfile)
import qualified Wasp.AppSpec.App.EmailSender as AS.EmailSender
import qualified Wasp.ExternalConfig.Npm.Dependency as Npm.Dependency
import qualified Wasp.SemanticVersion as SV

data EmailSenderProvider = EmailSenderProvider
  { npmDependency :: Maybe Npm.Dependency.Dependency,
    npmDevDependency :: Maybe Npm.Dependency.Dependency,
    setupFnFile :: Path' (Rel ProvidersDir) File'
  }
  deriving (Show, Eq)

data ProvidersDir

smtp :: EmailSenderProvider
smtp =
  EmailSenderProvider
    { npmDependency = Just nodeMailerDependency,
      npmDevDependency = Just nodeMailerTypesDependency,
      setupFnFile = [relfile|smtp.ts|]
    }
  where
    nodeMailerVersionRange :: SV.Range
    nodeMailerVersionRange = [SV.r|^8.0.1|]

    nodeMailerDependency :: Npm.Dependency.Dependency
    nodeMailerDependency = Npm.Dependency.make ("nodemailer", show nodeMailerVersionRange)

    nodeMailerTypesVersionRange :: SV.Range
    nodeMailerTypesVersionRange = [SV.r|^8.0.0|]

    nodeMailerTypesDependency :: Npm.Dependency.Dependency
    nodeMailerTypesDependency = Npm.Dependency.make ("@types/nodemailer", show nodeMailerTypesVersionRange)

sendGrid :: EmailSenderProvider
sendGrid =
  EmailSenderProvider
    { npmDependency = Just sendGridDependency,
      npmDevDependency = Nothing,
      setupFnFile = [relfile|sendgrid.ts|]
    }
  where
    sendGridVersionRange :: SV.Range
    sendGridVersionRange = [SV.r|^7.7.0|]

    sendGridDependency :: Npm.Dependency.Dependency
    sendGridDependency = Npm.Dependency.make ("@sendgrid/mail", show sendGridVersionRange)

mailgun :: EmailSenderProvider
mailgun =
  EmailSenderProvider
    { npmDependency = Just mailgunDependency,
      npmDevDependency = Nothing,
      setupFnFile = [relfile|mailgun.ts|]
    }
  where
    mailgunVersionRange :: SV.Range
    mailgunVersionRange = [SV.r|^10.2.3|]

    mailgunDependency :: Npm.Dependency.Dependency
    mailgunDependency = Npm.Dependency.make ("mailgun.js", show mailgunVersionRange)

resend :: EmailSenderProvider
resend =
  EmailSenderProvider
    { npmDependency = Just resendDependency,
      npmDevDependency = Nothing,
      setupFnFile = [relfile|resend.ts|]
    }
  where
    resendVersionRange :: SV.Range
    resendVersionRange = [SV.r|^6.0.0|]

    resendDependency :: Npm.Dependency.Dependency
    resendDependency = Npm.Dependency.make ("resend", show resendVersionRange)

dummy :: EmailSenderProvider
dummy =
  EmailSenderProvider
    { npmDependency = Nothing,
      npmDevDependency = Nothing,
      setupFnFile = [relfile|dummy.ts|]
    }

getEmailSenderProvider :: AS.EmailSender.EmailSender -> EmailSenderProvider
getEmailSenderProvider email = case AS.EmailSender.provider email of
  AS.EmailSender.SMTP -> smtp
  AS.EmailSender.SendGrid -> sendGrid
  AS.EmailSender.Mailgun -> mailgun
  AS.EmailSender.Resend -> resend
  AS.EmailSender.Dummy -> dummy
