module Wasp.Generator.SdkGenerator.UserCore
  ( genUserCoreTsconfigProject,
  )
where

import Data.Aeson (object, (.=))
import Data.Maybe (isJust)
import StrongPath (Dir, Path', Rel, fromRelDir, relfile)
import Wasp.AppSpec (AppSpec)
import qualified Wasp.AppSpec as AS
import qualified Wasp.AppSpec.App as AS.App
import qualified Wasp.AppSpec.App.Auth as AS.App.Auth
import qualified Wasp.AppSpec.App.Db as AS.Db
import Wasp.AppSpec.Valid (isAuthEnabled)
import qualified Wasp.AppSpec.Valid as AS.Valid
import Wasp.Generator.Common (WebAppRootDir, makeJsonWithEntityData)
import Wasp.Generator.DbGenerator (getEntitiesForPrismaSchema)
import qualified Wasp.Generator.DbGenerator.Auth as DbAuth
import Wasp.Generator.FileDraft (FileDraft)
import Wasp.Generator.Monad (Generator)
import Wasp.Generator.SdkGenerator.JsImport (extImportToImportJson)
import Wasp.Generator.SdkGenerator.UserCore.AuthG (genAuth)
import Wasp.Generator.SdkGenerator.UserCore.Client.AuthG (genNewClientAuth)
import Wasp.Generator.SdkGenerator.UserCore.Client.CrudG (genNewClientCrudApi)
import qualified Wasp.Generator.SdkGenerator.UserCore.Client.OperationsGenerator as ClientOpsGen
import Wasp.Generator.SdkGenerator.UserCore.Client.RouterGenerator (genNewClientRouterApi)
import Wasp.Generator.SdkGenerator.UserCore.Common
  ( mkTmplFd,
    mkTmplFdWithData,
  )
import Wasp.Generator.SdkGenerator.UserCore.CrudG (genCrud)
import Wasp.Generator.SdkGenerator.UserCore.EnvValidation (genEnvValidation)
import Wasp.Generator.SdkGenerator.UserCore.Server.AuthG (genNewServerApi)
import Wasp.Generator.SdkGenerator.UserCore.Server.CrudG (genNewServerCrudApi)
import Wasp.Generator.SdkGenerator.UserCore.Server.EmailSenderG (genNewEmailSenderApi)
import Wasp.Generator.SdkGenerator.UserCore.Server.JobGenerator (genNewJobsApi)
import qualified Wasp.Generator.SdkGenerator.UserCore.Server.OperationsGenerator as ServerOpsGen
import Wasp.Generator.SdkGenerator.UserCore.ServerApiG (genServerApi)
import Wasp.Generator.SdkGenerator.UserCore.WebSocketGenerator (genWebSockets)
import qualified Wasp.Generator.ServerGenerator.AuthG as AuthG
import qualified Wasp.Generator.ServerGenerator.Common as Server
import qualified Wasp.Generator.WebAppGenerator.Common as WebApp
import Wasp.Project.Common (WaspProjectDir, waspProjectDirFromAppComponentDir)
import qualified Wasp.Project.Db as Db
import Wasp.Util ((<++>))

genUserCoreTsconfigProject :: AppSpec -> Generator [FileDraft]
genUserCoreTsconfigProject spec =
  sequence
    [ return $ mkTmplFd [relfile|tsconfig.json|],
      return $ mkTmplFd [relfile|vite-env.d.ts|],
      return $ mkTmplFd [relfile|prisma-runtime-library.d.ts|],
      return $ mkTmplFd [relfile|server/index.ts|],
      return $ mkTmplFd [relfile|client/test/vitest/helpers.tsx|],
      return $ mkTmplFd [relfile|client/test/index.ts|],
      return $ mkTmplFd [relfile|client/hooks.ts|],
      return $ mkTmplFd [relfile|client/index.ts|],
      genClientConfigFile,
      genServerConfigFile spec,
      genServerUtils spec,
      genServerDbClient spec,
      genDevIndex
    ]
    <++> ServerOpsGen.genOperations spec
    <++> ClientOpsGen.genOperations spec
    <++> genAuth spec
    <++> genEntitiesAndServerTypesDirs spec
    <++> genCrud spec
    <++> genServerApi spec
    <++> genWebSockets spec
    <++> genServerMiddleware
    <++> genServerExportedTypesDir
    -- New API
    <++> genNewClientAuth spec
    <++> genNewServerApi spec
    <++> genNewServerCrudApi spec
    <++> genNewClientCrudApi spec
    <++> genNewEmailSenderApi spec
    <++> genNewJobsApi spec
    <++> genNewClientRouterApi spec
    <++> genEnvValidation spec

genClientConfigFile :: Generator FileDraft
genClientConfigFile =
  return $ mkTmplFdWithData tmplFile tmplData
  where
    tmplFile = [relfile|client/config.ts|]
    tmplData =
      object
        [ "serverUrlEnvVarName" .= WebApp.serverUrlEnvVarName
        ]

genServerConfigFile :: AppSpec -> Generator FileDraft
genServerConfigFile spec = return $ mkTmplFdWithData tmplFile tmplData
  where
    tmplFile = [relfile|server/config.ts|]
    tmplData =
      object
        [ "isAuthEnabled" .= isAuthEnabled spec,
          "clientUrlEnvVarName" .= Server.clientUrlEnvVarName,
          "serverUrlEnvVarName" .= Server.serverUrlEnvVarName,
          "jwtSecretEnvVarName" .= AuthG.jwtSecretEnvVarName,
          "databaseUrlEnvVarName" .= Db.databaseUrlEnvVarName
        ]

genServerUtils :: AppSpec -> Generator FileDraft
genServerUtils spec =
  return $ mkTmplFdWithData [relfile|server/utils.ts|] tmplData
  where
    tmplData = object ["isAuthEnabled" .= (isAuthEnabled spec :: Bool)]

genServerDbClient :: AppSpec -> Generator FileDraft
genServerDbClient spec = do
  areThereAnyEntitiesDefined <- not . null <$> getEntitiesForPrismaSchema spec
  let tmplData =
        object
          [ "areThereAnyEntitiesDefined" .= areThereAnyEntitiesDefined,
            "prismaSetupFn" .= extImportToImportJson maybePrismaSetupFn
          ]

  return $
    mkTmplFdWithData
      [relfile|server/dbClient.ts|]
      tmplData
  where
    maybePrismaSetupFn = AS.App.db (snd $ AS.Valid.getApp spec) >>= AS.Db.prismaSetupFn

genDevIndex :: Generator FileDraft
genDevIndex =
  return $ mkTmplFdWithData [relfile|dev/index.ts|] tmplData
  where
    tmplData = object ["waspProjectDirFromWebAppDir" .= fromRelDir waspProjectDirFromWebAppDir]
    waspProjectDirFromWebAppDir = waspProjectDirFromAppComponentDir :: Path' (Rel WebAppRootDir) (Dir WaspProjectDir)

genEntitiesAndServerTypesDirs :: AppSpec -> Generator [FileDraft]
genEntitiesAndServerTypesDirs spec =
  return
    [ entitiesIndexFileDraft,
      taggedEntitiesFileDraft,
      typesIndexFileDraft
    ]
  where
    entitiesIndexFileDraft =
      mkTmplFdWithData
        [relfile|entities/index.ts|]
        ( object
            [ "entities" .= allEntities,
              "isAuthEnabled" .= isJust maybeUserEntityName,
              "authEntityName" .= DbAuth.authEntityName,
              "authIdentityEntityName" .= DbAuth.authIdentityEntityName
            ]
        )
    taggedEntitiesFileDraft =
      mkTmplFdWithData
        [relfile|server/_types/taggedEntities.ts|]
        (object ["entities" .= allEntities])
    typesIndexFileDraft =
      mkTmplFdWithData
        [relfile|server/_types/index.ts|]
        ( object
            [ "entities" .= allEntities,
              "isAuthEnabled" .= isJust maybeUserEntityName
            ]
        )
    allEntities = map (makeJsonWithEntityData . fst) $ AS.getEntities spec
    maybeUserEntityName = AS.refName . AS.App.Auth.userEntity <$> AS.App.auth (snd $ AS.Valid.getApp spec)

genServerExportedTypesDir :: Generator [FileDraft]
genServerExportedTypesDir =
  return [mkTmplFd [relfile|server/types/index.ts|]]

genServerMiddleware :: Generator [FileDraft]
genServerMiddleware =
  sequence
    [ return $ mkTmplFd [relfile|server/middleware/index.ts|],
      return $ mkTmplFd [relfile|server/middleware/globalMiddleware.ts|]
    ]
