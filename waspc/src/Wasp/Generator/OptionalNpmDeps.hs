module Wasp.Generator.OptionalNpmDeps
  ( FeatureNpmDeps (..),
    getNpmDepsOfUsedFeatures,
    getAllDependencies,
    allOptionalNpmDeps,
  )
where

import Data.Maybe (isJust)
import Wasp.AppSpec (AppSpec)
import qualified Wasp.AppSpec.App as AS.App
import qualified Wasp.AppSpec.App.Auth as AS.Auth
import qualified Wasp.AppSpec.App.EmailSender as AS.EmailSender
import Wasp.AppSpec.Util (isPgBossJobExecutorUsed)
import Wasp.AppSpec.Valid (getApp)
import qualified Wasp.ExternalConfig.Npm.Dependency as Npm.Dependency
import Wasp.Generator.WebSocket (areWebSocketsUsed)

-- | A Wasp feature and the npm packages that the generated code imports only
-- when the app uses it.
data FeatureNpmDeps = FeatureNpmDeps
  { -- | Completes the sentence "Your app uses ...".
    featureName :: String,
    isFeatureUsed :: AppSpec -> Bool,
    dependencies :: [Npm.Dependency.Dependency],
    devDependencies :: [Npm.Dependency.Dependency]
  }

getAllDependencies :: FeatureNpmDeps -> [Npm.Dependency.Dependency]
getAllDependencies featureNpmDeps = dependencies featureNpmDeps ++ devDependencies featureNpmDeps

getNpmDepsOfUsedFeatures :: AppSpec -> [FeatureNpmDeps]
getNpmDepsOfUsedFeatures spec = filter (`isFeatureUsed` spec) featuresWithNpmDeps

-- | The optional peer dependencies of \@wasp/wasp. npm doesn't install them on
-- its own, so a project lists the ones of the features its app uses, and npm
-- checks their versions. A test keeps this list in sync with
-- @packages/wasp/package.json@.
allOptionalNpmDeps :: [Npm.Dependency.Dependency]
allOptionalNpmDeps = concatMap getAllDependencies featuresWithNpmDeps

featuresWithNpmDeps :: [FeatureNpmDeps]
featuresWithNpmDeps =
  [ FeatureNpmDeps
      { featureName = "auth",
        isFeatureUsed = isJust . getAuth,
        dependencies = Npm.Dependency.fromList [("lucia", "^3.0.1")],
        devDependencies = []
      },
    FeatureNpmDeps
      { featureName = "social auth",
        isFeatureUsed = maybe False AS.Auth.isExternalAuthEnabled . getAuth,
        dependencies = Npm.Dependency.fromList [("arctic", "^1.2.1")],
        devDependencies = []
      },
    FeatureNpmDeps
      { featureName = "jobs",
        isFeatureUsed = isPgBossJobExecutorUsed,
        -- NOTE: Our pg-boss related documentation references this version in URLs.
        -- Please update the docs when this changes (until we solve: https://github.com/wasp-lang/wasp/issues/596).
        dependencies = Npm.Dependency.fromList [("pg-boss", "^8.4.2")],
        devDependencies = []
      },
    FeatureNpmDeps
      { featureName = "WebSockets",
        isFeatureUsed = areWebSocketsUsed,
        dependencies =
          Npm.Dependency.fromList
            [ ("socket.io", "^4.6.1"),
              ("socket.io-client", "^4.6.1"),
              ("@socket.io/component-emitter", "^4.0.0")
            ],
        devDependencies = []
      },
    FeatureNpmDeps
      { featureName = "the SMTP email sender",
        isFeatureUsed = isEmailSenderProviderUsed AS.EmailSender.SMTP,
        dependencies = Npm.Dependency.fromList [("nodemailer", "^8.0.1")],
        devDependencies = Npm.Dependency.fromList [("@types/nodemailer", "^8.0.0")]
      },
    FeatureNpmDeps
      { featureName = "the SendGrid email sender",
        isFeatureUsed = isEmailSenderProviderUsed AS.EmailSender.SendGrid,
        dependencies = Npm.Dependency.fromList [("@sendgrid/mail", "^7.7.0")],
        devDependencies = []
      },
    FeatureNpmDeps
      { featureName = "the Mailgun email sender",
        isFeatureUsed = isEmailSenderProviderUsed AS.EmailSender.Mailgun,
        dependencies = Npm.Dependency.fromList [("mailgun.js", "^10.2.3")],
        devDependencies = []
      },
    FeatureNpmDeps
      { featureName = "the Resend email sender",
        isFeatureUsed = isEmailSenderProviderUsed AS.EmailSender.Resend,
        dependencies = Npm.Dependency.fromList [("resend", "^6.0.0")],
        devDependencies = []
      }
  ]
  where
    getAuth = AS.App.auth . getApp

    isEmailSenderProviderUsed provider spec =
      (AS.EmailSender.provider <$> AS.App.emailSender (getApp spec)) == Just provider
