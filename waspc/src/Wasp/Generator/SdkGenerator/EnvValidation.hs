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
import Wasp.Generator.JsImport (virtualExtImportToImportJson)
import Wasp.Generator.Monad (Generator)
import Wasp.Generator.SdkGenerator.Client.VitePlugin.Common (userClientEnvSchemaVF)
import Wasp.Generator.SdkGenerator.Common (genFileCopy, mkTmplFdWithData)
import qualified Wasp.Generator.ServerGenerator.AuthG as AuthG
import qualified Wasp.Generator.ServerGenerator.Common as Server
import Wasp.Generator.ServerGenerator.VirtualFiles (userServerEnvSchemaVF)
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

genServerEnvFiles :: AppSpec -> Generator [FileDraft]
genServerEnvFiles spec =
  sequence
    [ genServerEnv spec
    ]
    <++> genUserServerEnvSchemaModuleDecl spec

genClientEnvFiles :: AppSpec -> Generator [FileDraft]
genClientEnvFiles spec =
  sequence
    [ genClientEnvSchema spec,
      genFileCopy [relfile|client/env.ts|]
    ]
    <++> genUserClientEnvSchemaModuleDecl spec

genServerEnv :: AppSpec -> Generator FileDraft
genServerEnv spec = return $ mkTmplFdWithData [relfile|server/env.ts|] tmplData
  where
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
          "userServerEnvSchema" .= virtualExtImportToImportJson userServerEnvSchemaVF maybeServerEnvValidationSchema
        ]
    maybeAuth = AS.App.auth app
    maybeEmailSender = AS.App.emailSender app
    app = snd $ getApp spec
    maybeServerEnvValidationSchema = AS.App.server app >>= AS.App.Server.envValidationSchema

genClientEnvSchema :: AppSpec -> Generator FileDraft
genClientEnvSchema spec = return $ mkTmplFdWithData tmplPath tmplData
  where
    tmplPath = [relfile|client/env/schema.ts|]
    tmplData =
      object
        [ "serverUrlEnvVarName" .= WebApp.serverUrlEnvVarName,
          "defaultServerUrl" .= Server.defaultDevServerUrl,
          "userClientEnvSchema" .= virtualExtImportToImportJson userClientEnvSchemaVF maybeClientEnvValidationSchema
        ]
    maybeClientEnvValidationSchema = AS.App.client (snd $ getApp spec) >>= AS.App.Client.envValidationSchema

genUserClientEnvSchemaModuleDecl :: AppSpec -> Generator [FileDraft]
genUserClientEnvSchemaModuleDecl spec
  | isJust maybeClientEnvSchema =
      return
        [ mkTmplFdWithData
            [relfile|client/env/userClientEnvSchema.d.ts|]
            (object ["userClientEnvSchema" .= userClientEnvSchemaImportJson])
        ]
  | otherwise = return []
  where
    userClientEnvSchemaImportJson = virtualExtImportToImportJson userClientEnvSchemaVF maybeClientEnvSchema
    maybeClientEnvSchema = AS.App.client (snd $ getApp spec) >>= AS.App.Client.envValidationSchema

genUserServerEnvSchemaModuleDecl :: AppSpec -> Generator [FileDraft]
genUserServerEnvSchemaModuleDecl spec
  | isJust maybeUserServerEnvSchema =
      return
        [ mkTmplFdWithData
            [relfile|server/userServerEnvSchema.d.ts|]
            (object ["userServerEnvSchema" .= userServerEnvSchemaImportJson])
        ]
  | otherwise = return []
  where
    userServerEnvSchemaImportJson = virtualExtImportToImportJson userServerEnvSchemaVF maybeUserServerEnvSchema
    maybeUserServerEnvSchema = AS.App.server (snd $ getApp spec) >>= AS.App.Server.envValidationSchema

depsRequiredByEnvValidation :: [Npm.Dependency.Dependency]
depsRequiredByEnvValidation =
  Npm.Dependency.fromList
    [ ("zod", "^4.3.6")
    ]
