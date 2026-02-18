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
import Wasp.AppSpec.Valid (getApp, getIdFieldFromCrudEntity)
import Wasp.Generator.Crud (crudDeclarationToOperationsList, getCrudFilePath, getCrudOperationJson, makeCrudOperationKeyAndJsonPair)
import Wasp.Generator.FileDraft (FileDraft)
import Wasp.Generator.Monad (Generator)
import Wasp.Generator.TypesGenerator.Common (mkTmplFdWithData, mkTmplFdWithDstAndData)
import Wasp.Generator.TypesGenerator.JsImport (extImportToImportJson, extOperationImportToImportJson)
import Wasp.Util ((<++>))

genTypes :: AppSpec -> Generator [FileDraft]
genTypes spec =
  genConfigTypes spec
    <++> genCrudTypes spec
    <++> genOperationTypes spec

genConfigTypes :: AppSpec -> Generator [FileDraft]
genConfigTypes spec =
  return
    [ mkTmplFdWithData
        [relfile|configuration.d.ts|]
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
genCrudTypes spec = return $ map genCrudType $ getCruds spec
  where
    genCrudType :: (String, AS.Crud.Crud) -> FileDraft
    genCrudType (name, crud) =
      mkTmplFdWithDstAndData
        [relfile|_crudTypes.d.ts|]
        (getCrudFilePath ("crud" ++ name) "d.ts")
        (Just tmplData)
      where
        tmplData =
          object
            [ "crud" .= getCrudOperationJson name crud idField,
              "overrides" .= object overrides
            ]
        idField = getIdFieldFromCrudEntity spec crud

        overrides :: [Aeson.Types.Pair]
        overrides = map operationToOverrideImport crudOperations

        crudOperations = crudDeclarationToOperationsList crud

        operationToOverrideImport :: (AS.Crud.CrudOperation, AS.Crud.CrudOperationOptions) -> Aeson.Types.Pair
        operationToOverrideImport (operation, options) = makeCrudOperationKeyAndJsonPair operation importJson
          where
            importJson = extImportToImportJson $ AS.Crud.overrideFn options

genOperationTypes :: AppSpec -> Generator [FileDraft]
genOperationTypes spec =
  return $ genQueryTypes ++ genActionTypes
  where
    genQueryTypes :: [FileDraft]
    genQueryTypes
      | null queries = []
      | otherwise =
          [ mkTmplFdWithDstAndData
              [relfile|_operationTypes.d.ts|]
              [relfile|operationQueries.d.ts|]
              (Just tmplData)
          ]
      where
        tmplData =
          object
            [ "operations" .= map (mkOperationData . uncurry AS.Operation.QueryOp) queries
            ]
        queries = getQueries spec

    genActionTypes :: [FileDraft]
    genActionTypes
      | null actions = []
      | otherwise =
          [ mkTmplFdWithDstAndData
              [relfile|_operationTypes.d.ts|]
              [relfile|operationActions.d.ts|]
              (Just tmplData)
          ]
      where
        tmplData =
          object
            [ "operations" .= map (mkOperationData . uncurry AS.Operation.ActionOp) actions
            ]
        actions = getActions spec

    mkOperationData :: AS.Operation.Operation -> Aeson.Types.Value
    mkOperationData operation =
      object
        [ "jsFn" .= extOperationImportToImportJson (AS.Operation.getFn operation),
          "operationName" .= AS.Operation.getName operation
        ]
