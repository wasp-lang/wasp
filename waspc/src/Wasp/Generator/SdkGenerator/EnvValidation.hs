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
import qualified Wasp.AppSpec.App.Client as AS.App.Client
import qualified Wasp.AppSpec.App.Server as AS.App.Server
import Wasp.AppSpec.Valid (getApp)
import qualified Wasp.ExternalConfig.Npm.Dependency as Npm.Dependency
import qualified Wasp.Generator.AuthProviders as AuthProviders
import qualified Wasp.Generator.EmailSenders as EmailSenders
import Wasp.Generator.FileDraft (FileDraft)
import Wasp.Generator.Monad (Generator)
import qualified Wasp.Generator.SdkGenerator.Common as C
import Wasp.Generator.SdkGenerator.JsImport (extImportToImportJson)
import qualified Wasp.Generator.ServerGenerator.AuthG as AuthG
import qualified Wasp.Generator.ServerGenerator.Common as Server
import qualified Wasp.Generator.WebAppGenerator.Common as WebApp
import qualified Wasp.Project.Db as Db
import Wasp.Util ((<++>))

genEnvValidation :: AppSpec -> Generator [FileDraft]
genEnvValidation spec =
  genSharedEnvFiles
    <++> genServerEnvFiles spec
    <++> genClientEnvFiles spec

genSharedEnvFiles :: Generator [FileDraft]
genSharedEnvFiles =
  sequence
    [ genFileCopy [relfile|env/index.ts|],
      genFileCopy [relfile|env/validation.ts|]
    ]
  where
    genFileCopy = return . C.mkTmplFd

genServerEnvFiles :: AppSpec -> Generator [FileDraft]
genServerEnvFiles spec = sequence [genServerEnv spec]

genClientEnvFiles :: AppSpec -> Generator [FileDraft]
genClientEnvFiles spec =
  sequence
    [ genClientEnvSchema spec,
      genFileCopy [relfile|client/env.ts|]
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
          "clientUrlEnvVarName" .= Server.clientUrlEnvVarName,
          "serverUrlEnvVarName" .= Server.serverUrlEnvVarName,
          "jwtSecretEnvVarName" .= AuthG.jwtSecretEnvVarName,
          "databaseUrlEnvVarName" .= Db.databaseUrlEnvVarName,
          "defaultClientUrl" .= WebApp.getDefaultDevClientUrl spec,
          "defaultServerUrl" .= Server.defaultDevServerUrl,
          "defaultServerPort" .= Server.defaultServerPort,
          "enabledAuthProviders" .= (AuthProviders.getEnabledAuthProvidersJson <$> maybeAuth),
          "isEmailSenderEnabled" .= isJust maybeEmailSender,
          "enabledEmailSenders" .= (EmailSenders.getEnabledEmailProvidersJson <$> maybeEmailSender),
          "envValidationSchema" .= extImportToImportJson maybeEnvValidationSchema
        ]
    maybeAuth = AS.App.auth app
    maybeEmailSender = AS.App.emailSender app
    maybeEnvValidationSchema = AS.App.server app >>= AS.App.Server.envValidationSchema
    app = snd $ getApp spec

genClientEnvSchema :: AppSpec -> Generator FileDraft
genClientEnvSchema spec = return $ C.mkTmplFdWithData tmplPath tmplData
  where
    tmplPath = [relfile|client/env/schema.ts|]
    tmplData =
      object
        [ "serverUrlEnvVarName" .= WebApp.serverUrlEnvVarName,
          "defaultServerUrl" .= Server.defaultDevServerUrl,
          "envValidationSchema" .= extImportToImportJson maybeEnvValidationSchema
        ]
    maybeEnvValidationSchema = AS.App.client app >>= AS.App.Client.envValidationSchema
    app = snd $ getApp spec

depsRequiredByEnvValidation :: [Npm.Dependency.Dependency]
depsRequiredByEnvValidation =
  Npm.Dependency.fromList
    [ ("zod", "^3.23.8")
    ]
