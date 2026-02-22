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
import Wasp.AppSpec.Valid (getApp)
import qualified Wasp.ExternalConfig.Npm.Dependency as Npm.Dependency
import qualified Wasp.Generator.AuthProviders as AuthProviders
import qualified Wasp.Generator.EmailSenders as EmailSenders
import Wasp.Generator.FileDraft (FileDraft)
import Wasp.Generator.Monad (Generator)
import Wasp.Generator.SdkGenerator.Client.VitePlugin.Common (userClientEnvSchemaPath)
import Wasp.Generator.SdkGenerator.Common (genFileCopy, mkTmplFdWithData)
import qualified Wasp.Generator.ServerGenerator.AuthG as AuthG
import qualified Wasp.Generator.ServerGenerator.Common as Server
import qualified Wasp.Generator.WebAppGenerator.Common as WebApp
import qualified Wasp.Project.Db as Db
import Wasp.Util ((<++>))

genEnvValidation :: AppSpec -> Generator [FileDraft]
genEnvValidation spec =
  genSharedEnvFiles
    <++> genServerEnvFiles spec
    <++> genClientEnvFiles

genSharedEnvFiles :: Generator [FileDraft]
genSharedEnvFiles =
  sequence
    [ genFileCopy [relfile|env/index.ts|],
      genFileCopy [relfile|env/validation.ts|]
    ]

genServerEnvFiles :: AppSpec -> Generator [FileDraft]
genServerEnvFiles spec = sequence [genServerEnv spec]

genClientEnvFiles :: Generator [FileDraft]
genClientEnvFiles =
  sequence
    [ genClientEnvSchema,
      genClientEnvSchemaType,
      genFileCopy [relfile|client/env.ts|]
    ]

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
          "enabledEmailSenders" .= (EmailSenders.getEnabledEmailProvidersJson <$> maybeEmailSender)
        ]
    maybeAuth = AS.App.auth app
    maybeEmailSender = AS.App.emailSender app
    app = snd $ getApp spec

genClientEnvSchema :: Generator FileDraft
genClientEnvSchema = return $ mkTmplFdWithData tmplPath tmplData
  where
    tmplPath = [relfile|client/env/schema.ts|]
    tmplData =
      object
        [ "serverUrlEnvVarName" .= WebApp.serverUrlEnvVarName,
          "defaultServerUrl" .= Server.defaultDevServerUrl,
          "userClientEnvSchemaPath" .= userClientEnvSchemaPath
        ]

genClientEnvSchemaType :: Generator FileDraft
genClientEnvSchemaType = return $ mkTmplFdWithData tmplPath tmplData
  where
    tmplPath = [relfile|client/env/userClientEnvSchema.d.ts|]
    tmplData =
      object
        [ "userClientEnvSchemaPath" .= userClientEnvSchemaPath
        ]

depsRequiredByEnvValidation :: [Npm.Dependency.Dependency]
depsRequiredByEnvValidation =
  Npm.Dependency.fromList
    [ ("zod", "^3.23.8")
    ]
