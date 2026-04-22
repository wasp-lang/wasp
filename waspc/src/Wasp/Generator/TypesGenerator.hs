module Wasp.Generator.TypesGenerator
  ( genTypes,
  )
where

import Data.Aeson (object, (.=))
import qualified Data.Aeson.Types as Aeson.Types
import StrongPath (relfile)
import Wasp.AppSpec (AppSpec, getActions, getCruds, getQueries)
import qualified Wasp.AppSpec.App as AS.App
import qualified Wasp.AppSpec.App.Auth as AS.Auth
import qualified Wasp.AppSpec.App.Client as AS.App.Client
import qualified Wasp.AppSpec.App.Db as AS.Db
import qualified Wasp.AppSpec.App.Server as AS.App.Server
import qualified Wasp.AppSpec.App.WebSocket as AS.App.WS
import qualified Wasp.AppSpec.Crud as AS.Crud
import qualified Wasp.AppSpec.Operation as AS.Operation
import Wasp.AppSpec.Valid (getApp)
import Wasp.Generator.Crud (crudDeclarationToOperationsList, makeCrudOperationKeyAndJsonPair)
import Wasp.Generator.FileDraft (FileDraft)
import Wasp.Generator.Monad (Generator)
import Wasp.Generator.TypesGenerator.Common (mkTmplFd, mkTmplFdWithData)
import Wasp.Generator.TypesGenerator.JsImport (extImportToImportJson, extOperationImportToImportJson)
import Wasp.Util ((<++>))

genTypes :: AppSpec -> Generator [FileDraft]
genTypes spec =
  return [mkTmplFd [relfile|wasp-user-virtual-modules.d.ts|]]
    <++> genConfigTypes spec
    <++> genCrudTypes spec
    <++> genOperationTypes spec

genConfigTypes :: AppSpec -> Generator [FileDraft]
genConfigTypes spec =
  return
    [ mkTmplFdWithData
        [relfile|configuration.mts|]
        tmplData
    ]
  where
    tmplData =
      object
        [ "serverEnvSchema" .= extImportToImportJson (AS.App.server app >>= AS.App.Server.envValidationSchema),
          "clientEnvSchema" .= extImportToImportJson (AS.App.client app >>= AS.App.Client.envValidationSchema),
          "webSocketFn" .= extImportToImportJson (AS.App.WS.fn <$> AS.App.webSocket app),
          "prismaSetupFn" .= extImportToImportJson (AS.App.db app >>= AS.Db.prismaSetupFn),
          "emailUserSignupFields" .= extImportToImportJson (authMethods >>= AS.Auth.email >>= AS.Auth.userSignupFieldsForEmailAuth),
          "usernameAndPasswordUserSignupFields" .= extImportToImportJson (authMethods >>= AS.Auth.usernameAndPassword >>= AS.Auth.userSignupFieldsForUsernameAuth)
        ]
    authMethods = AS.Auth.methods <$> maybeAuth
    maybeAuth = AS.App.auth app
    app = snd $ getApp spec

genCrudTypes :: AppSpec -> Generator [FileDraft]
genCrudTypes spec
  | null cruds = return []
  | otherwise =
      return
        [ mkTmplFdWithData
            [relfile|crud.mts|]
            (object ["cruds" .= map mkCrudData cruds])
        ]
  where
    cruds = getCruds spec

    mkCrudData :: (String, AS.Crud.Crud) -> Aeson.Types.Value
    mkCrudData (name, crud) =
      object
        [ "name" .= name,
          "overrides" .= object (map operationToOverrideImport (crudDeclarationToOperationsList crud))
        ]

    operationToOverrideImport :: (AS.Crud.CrudOperation, AS.Crud.CrudOperationOptions) -> Aeson.Types.Pair
    operationToOverrideImport (operation, options) =
      makeCrudOperationKeyAndJsonPair operation (extImportToImportJson (AS.Crud.overrideFn options))

genOperationTypes :: AppSpec -> Generator [FileDraft]
genOperationTypes spec
  | null operations = return []
  | otherwise =
      return
        [ mkTmplFdWithData
            [relfile|operations.mts|]
            (object ["operations" .= map mkOperationData operations])
        ]
  where
    operations =
      map (uncurry AS.Operation.QueryOp) (getQueries spec)
        ++ map (uncurry AS.Operation.ActionOp) (getActions spec)

    mkOperationData :: AS.Operation.Operation -> Aeson.Types.Value
    mkOperationData operation =
      object
        [ "jsFn" .= extOperationImportToImportJson (AS.Operation.getFn operation),
          "operationName" .= AS.Operation.getName operation
        ]
