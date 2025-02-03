module Wasp.Generator.SdkGenerator.EnvValidation
  ( genEnvValidation,
    depsRequiredByEnvValidation,
  )
where

import Data.Aeson (KeyValue ((.=)), object)
import Data.Maybe (isJust)
import StrongPath (relfile)
import Wasp.AppSpec (AppSpec)
import qualified Wasp.AppSpec.App as AS.App
import qualified Wasp.AppSpec.App.Auth as AS.Auth
import qualified Wasp.AppSpec.App.Client as AS.App.Client
import qualified Wasp.AppSpec.App.Dependency as AS.Dependency
import qualified Wasp.AppSpec.App.Server as AS.App.Server
import Wasp.AppSpec.Valid (getApp)
import qualified Wasp.Generator.AuthProviders as AuthProviders
import qualified Wasp.Generator.EmailSenders as EmailSenders
import Wasp.Generator.FileDraft (FileDraft)
import qualified Wasp.Generator.JsImport as GJI
import Wasp.Generator.Monad (Generator)
import qualified Wasp.Generator.SdkGenerator.Common as C
import Wasp.Generator.SdkGenerator.Server.OperationsGenerator (extImportToJsImport)
import qualified Wasp.Generator.ServerGenerator.Common as Server
import qualified Wasp.Generator.WebAppGenerator.Common as WebApp
import qualified Wasp.Project.Db as Db

genEnvValidation :: AppSpec -> Generator [FileDraft]
genEnvValidation spec =
  sequence
    [ genServerEnv spec,
      genClientEnv spec,
      genFileCopy [relfile|env/index.ts|],
      genFileCopy [relfile|env/validation.ts|]
    ]
  where
    genFileCopy = return . C.mkTmplFd

genServerEnv :: AppSpec -> Generator FileDraft
genServerEnv spec = return $ C.mkTmplFdWithData tmplPath tmplData
  where
    tmplPath = [relfile|server/env.ts|]
    tmplData =
      object
        [ "isAuthEnabled" .= isJust maybeAuth,
          "isCookieAuthEnabled" .= maybe False AS.Auth.isCookieAuthEnabled maybeAuth,
          "databaseUrlEnvVarName" .= Db.databaseUrlEnvVarName,
          "defaultClientUrl" .= WebApp.getDefaultDevClientUrl spec,
          "defaultServerUrl" .= Server.defaultDevServerUrl,
          "defaultServerPort" .= Server.defaultServerPort,
          "enabledAuthProviders" .= (AuthProviders.getEnabledAuthProvidersJson <$> maybeAuth),
          "isEmailSenderEnabled" .= isJust maybeEmailSender,
          "enabledEmailSenders" .= (EmailSenders.getEnabledEmailProvidersJson <$> maybeEmailSender),
          "envValidationSchema" .= GJI.jsImportToImportJson (extImportToJsImport <$> maybeEnvValidationSchema)
        ]
    maybeAuth = AS.App.auth app
    maybeEmailSender = AS.App.emailSender app
    maybeEnvValidationSchema = AS.App.server app >>= AS.App.Server.envValidationSchema
    app = snd $ getApp spec

genClientEnv :: AppSpec -> Generator FileDraft
genClientEnv spec = return $ C.mkTmplFdWithData tmplPath tmplData
  where
    tmplPath = [relfile|client/env.ts|]
    tmplData =
      object
        [ "defaultServerUrl" .= Server.defaultDevServerUrl,
          "envValidationSchema" .= GJI.jsImportToImportJson (extImportToJsImport <$> maybeEnvValidationSchema)
        ]
    maybeEnvValidationSchema = AS.App.client app >>= AS.App.Client.envValidationSchema
    app = snd $ getApp spec

depsRequiredByEnvValidation :: [AS.Dependency.Dependency]
depsRequiredByEnvValidation =
  AS.Dependency.fromList
    [ ("zod", "^3.23.8")
    ]
