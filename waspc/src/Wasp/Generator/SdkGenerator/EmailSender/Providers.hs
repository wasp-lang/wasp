module Wasp.Generator.SdkGenerator.EmailSender.Providers
  ( EmailSenderProvider (..),
    ProvidersDir,
    getEmailSenderProvider,
    emailSendersNpmDeps,
  )
where

import StrongPath (File', Path', Rel, relfile)
import Wasp.AppSpec (AppSpec)
import qualified Wasp.AppSpec.App as AS.App
import qualified Wasp.AppSpec.App.EmailSender as AS.EmailSender
import Wasp.AppSpec.Valid (getApp)
import qualified Wasp.ExternalConfig.Npm.Dependency as Npm.Dependency
import Wasp.Generator.OptionalFeatureNpmDeps.Common (OptionalFeatureNpmDeps (OptionalFeatureNpmDeps))
import qualified Wasp.Generator.OptionalFeatureNpmDeps.Common as OptionalFeatureNpmDeps
import qualified Wasp.SemanticVersion as SV

data EmailSenderProvider = EmailSenderProvider
  { -- | Completes the sentence "Your app uses ...".
    featureDescription :: String,
    npmDependencies :: [Npm.Dependency.Dependency],
    npmDevDependencies :: [Npm.Dependency.Dependency],
    setupFnFile :: Path' (Rel ProvidersDir) File'
  }

data ProvidersDir

getEmailSenderProvider :: AS.EmailSender.EmailProvider -> EmailSenderProvider
getEmailSenderProvider = \case
  AS.EmailSender.SMTP -> smtp
  AS.EmailSender.SendGrid -> sendGrid
  AS.EmailSender.Mailgun -> mailgun
  AS.EmailSender.Resend -> resend
  AS.EmailSender.Dummy -> dummy

emailSendersNpmDeps :: [OptionalFeatureNpmDeps.OptionalFeatureNpmDeps]
emailSendersNpmDeps =
  [ OptionalFeatureNpmDeps
      { OptionalFeatureNpmDeps.featureDescription = featureDescription emailSenderProvider,
        OptionalFeatureNpmDeps.isFeatureUsed = isEmailProviderUsed emailProvider,
        OptionalFeatureNpmDeps.dependencies = npmDependencies emailSenderProvider,
        OptionalFeatureNpmDeps.devDependencies = npmDevDependencies emailSenderProvider
      }
  | emailProvider <- [minBound .. maxBound],
    let emailSenderProvider = getEmailSenderProvider emailProvider
  ]
  where
    isEmailProviderUsed :: AS.EmailSender.EmailProvider -> AppSpec -> Bool
    isEmailProviderUsed emailProvider spec =
      (AS.EmailSender.provider <$> AS.App.emailSender (getApp spec)) == Just emailProvider

smtp :: EmailSenderProvider
smtp =
  EmailSenderProvider
    { featureDescription = "the SMTP email sender",
      npmDependencies = [Npm.Dependency.make ("nodemailer", show [SV.r|^8.0.1|])],
      npmDevDependencies = [Npm.Dependency.make ("@types/nodemailer", show [SV.r|^8.0.0|])],
      setupFnFile = [relfile|smtp.ts|]
    }

sendGrid :: EmailSenderProvider
sendGrid =
  EmailSenderProvider
    { featureDescription = "the SendGrid email sender",
      npmDependencies = [Npm.Dependency.make ("@sendgrid/mail", show [SV.r|^7.7.0|])],
      npmDevDependencies = [],
      setupFnFile = [relfile|sendgrid.ts|]
    }

mailgun :: EmailSenderProvider
mailgun =
  EmailSenderProvider
    { featureDescription = "the Mailgun email sender",
      npmDependencies = [Npm.Dependency.make ("mailgun.js", show [SV.r|^10.2.3|])],
      npmDevDependencies = [],
      setupFnFile = [relfile|mailgun.ts|]
    }

resend :: EmailSenderProvider
resend =
  EmailSenderProvider
    { featureDescription = "the Resend email sender",
      npmDependencies = [Npm.Dependency.make ("resend", show [SV.r|^6.0.0|])],
      npmDevDependencies = [],
      setupFnFile = [relfile|resend.ts|]
    }

dummy :: EmailSenderProvider
dummy =
  EmailSenderProvider
    { featureDescription = "the Dummy email sender",
      npmDependencies = [],
      npmDevDependencies = [],
      setupFnFile = [relfile|dummy.ts|]
    }
