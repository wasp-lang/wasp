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
import qualified Wasp.AppSpec.App.Dependency as AS.Dependency
import Wasp.AppSpec.Valid (getApp)
import qualified Wasp.Generator.AuthProviders as AuthProviders
import qualified Wasp.Generator.EmailSenders as EmailSenders
import Wasp.Generator.FileDraft (FileDraft)
import Wasp.Generator.Monad (Generator)
import qualified Wasp.Generator.SdkGenerator.Common as C
import qualified Wasp.Generator.ServerGenerator.Common as Server
import qualified Wasp.Generator.WebAppGenerator.Common as WebApp
import qualified Wasp.Project.Db as Db

genEnvValidation :: AppSpec -> Generator [FileDraft]
genEnvValidation spec =
  sequence
    [ genServerEnv spec,
      genClientEnv,
      genFileCopy [relfile|env/index.ts|]
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
          "databaseUrlEnvVarName" .= Db.databaseUrlEnvVarName,
          "defaultClientUrl" .= WebApp.getDefaultDevClientUrl spec,
          "defaultServerUrl" .= Server.defaultDevServerUrl,
          "defaultServerPort" .= Server.defaultServerPort,
          "enabledAuthProviders" .= (AuthProviders.getEnabledAuthProvidersJson <$> maybeAuth),
          "isEmailSenderUsed" .= isJust maybeEmailSender,
          "enabledEmailSenders" .= (EmailSenders.getEnabledEmailProvidersJson <$> maybeEmailSender)
        ]
    maybeAuth = AS.App.auth app
    maybeEmailSender = AS.App.emailSender app
    app = snd $ getApp spec

genClientEnv :: Generator FileDraft
genClientEnv = return $ C.mkTmplFdWithData tmplPath tmplData
  where
    tmplPath = [relfile|client/env.ts|]
    tmplData = object ["defaultServerUrl" .= Server.defaultDevServerUrl]

depsRequiredByEnvValidation :: [AS.Dependency.Dependency]
depsRequiredByEnvValidation =
  AS.Dependency.fromList
    [ ("zod", "^3.23.8")
    ]
