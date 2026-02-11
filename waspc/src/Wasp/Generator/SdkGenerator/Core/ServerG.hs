{-# LANGUAGE OverloadedRecordDot #-}

module Wasp.Generator.SdkGenerator.Core.ServerG
  ( genServer,
  )
where

import Data.Aeson (KeyValue ((.=)), object, (.=))
import Data.Maybe (isJust)
import StrongPath (relfile)
import Wasp.AppSpec (AppSpec)
import qualified Wasp.AppSpec as AS
import qualified Wasp.AppSpec.App as AS.App
import Wasp.AppSpec.Valid (getApp)
import qualified Wasp.Generator.AuthProviders as AuthProviders
import Wasp.Generator.Common (makeJsonWithEntityData)
import qualified Wasp.Generator.EmailSenders as EmailSenders
import Wasp.Generator.FileDraft (FileDraft)
import Wasp.Generator.Monad (Generator)
import Wasp.Generator.SdkGenerator.Core.Common (mkTmplFd, mkTmplFdWithData)
import Wasp.Generator.SdkGenerator.Core.Server.AuthG (genServerAuth)
import Wasp.Generator.SdkGenerator.Core.Server.EmailG (genServerEmail)
import qualified Wasp.Generator.ServerGenerator.AuthG as AuthG
import qualified Wasp.Generator.ServerGenerator.Common as Server
import qualified Wasp.Generator.WebAppGenerator.Common as WebApp
import qualified Wasp.Project.Db as Db
import Wasp.Util ((<++>))

genServer :: AppSpec -> Generator [FileDraft]
genServer spec =
  return
    [ mkTmplFd [relfile|server/HttpError.ts|],
      mkTmplFd [relfile|server/types/index.ts|],
      mkTmplFd [relfile|server/jobs/core/job.ts|]
    ]
    <++> genServerMiddleware
    <++> genServerTaggedEntities spec
    <++> genServerAuth spec
    <++> genServerEmail spec
    <++> genServerWaspEnv spec

genServerMiddleware :: Generator [FileDraft]
genServerMiddleware =
  return
    [ mkTmplFd [relfile|server/middleware/index.ts|],
      mkTmplFd [relfile|server/middleware/globalMiddleware.ts|]
    ]

genServerTaggedEntities :: AppSpec -> Generator [FileDraft]
genServerTaggedEntities spec =
  return [mkTmplFdWithData [relfile|server/_types/taggedEntities.ts|] tmplData]
  where
    tmplData = object ["entities" .= allEntities]
    allEntities = map (makeJsonWithEntityData . fst) $ AS.getEntities spec

genServerWaspEnv :: AppSpec -> Generator [FileDraft]
genServerWaspEnv spec = return [mkTmplFdWithData [relfile|server/waspEnv.ts|] tmplData]
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
