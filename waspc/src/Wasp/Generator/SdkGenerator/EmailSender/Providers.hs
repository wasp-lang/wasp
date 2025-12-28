module Wasp.Generator.SdkGenerator.EmailSender.Providers
  ( smtp,
    sendGrid,
    mailgun,
    dummy,
    serverProvidersDirInSdkTemplatesDir,
    EmailSenderProvider (..),
  )
where

import StrongPath (Dir, File', Path', Rel, reldir, relfile, (</>))
import qualified Wasp.ExternalConfig.Npm.Dependency as Npm.Dependency
import Wasp.Generator.SdkGenerator.Common (SdkTemplatesProjectDir)
import Wasp.Generator.SdkGenerator.Server.Common (serverTemplatesDirInSdkTemplatesDir)
import qualified Wasp.SemanticVersion as SV

data ServerProvidersTemplatesDir

data EmailSenderProvider = EmailSenderProvider
  { npmDependency :: Maybe Npm.Dependency.Dependency,
    setupFnFile :: Path' (Rel ServerProvidersTemplatesDir) File'
  }
  deriving (Show, Eq)

smtp :: EmailSenderProvider
smtp =
  EmailSenderProvider
    { npmDependency = Just nodeMailerDependency,
      setupFnFile = [relfile|smtp.ts|]
    }
  where
    nodeMailerVersionRange :: SV.Range
    nodeMailerVersionRange = SV.Range [SV.backwardsCompatibleWith (SV.Version 6 9 1)]

    nodeMailerDependency :: Npm.Dependency.Dependency
    nodeMailerDependency = Npm.Dependency.make ("nodemailer", show nodeMailerVersionRange)

sendGrid :: EmailSenderProvider
sendGrid =
  EmailSenderProvider
    { npmDependency = Just sendGridDependency,
      setupFnFile = [relfile|sendgrid.ts|]
    }
  where
    sendGridVersionRange :: SV.Range
    sendGridVersionRange = SV.Range [SV.backwardsCompatibleWith (SV.Version 7 7 0)]

    sendGridDependency :: Npm.Dependency.Dependency
    sendGridDependency = Npm.Dependency.make ("@sendgrid/mail", show sendGridVersionRange)

mailgun :: EmailSenderProvider
mailgun =
  EmailSenderProvider
    { npmDependency = Just mailgunDependency,
      setupFnFile = [relfile|mailgun.ts|]
    }
  where
    mailgunVersionRange :: SV.Range
    mailgunVersionRange = SV.Range [SV.backwardsCompatibleWith (SV.Version 10 2 3)]

    mailgunDependency :: Npm.Dependency.Dependency
    mailgunDependency = Npm.Dependency.make ("mailgun.js", show mailgunVersionRange)

dummy :: EmailSenderProvider
dummy =
  EmailSenderProvider
    { npmDependency = Nothing,
      setupFnFile = [relfile|dummy.ts|]
    }

serverProvidersDirInSdkTemplatesDir :: Path' (Rel SdkTemplatesProjectDir) (Dir ServerProvidersTemplatesDir)
serverProvidersDirInSdkTemplatesDir = serverTemplatesDirInSdkTemplatesDir </> [reldir|email/core/providers|]
