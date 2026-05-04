module Wasp.Generator.UserTypesGenerator
  ( genUserTypes,
  )
where

import Data.Aeson (object, (.=))
import qualified Data.Aeson.Types as Aeson.Types
import StrongPath (relfile)
import Wasp.AppSpec (AppSpec, getCruds, getOperations)
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
import Wasp.Generator.UserTypesGenerator.Common (mkTmplFdWithData)
import Wasp.Generator.UserTypesGenerator.JsImport (extImportToImportJson, extOperationImportToImportJson)
import Wasp.Util ((<++>))

genUserTypes :: AppSpec -> Generator [FileDraft]
genUserTypes spec = genUserModuleAugmentation spec

genUserModuleAugmentation :: AppSpec -> Generator [FileDraft]
genUserModuleAugmentation spec =
  genRegister spec
    <++> genCrudRegister spec
    <++> genOperationRegister spec

genRegister :: AppSpec -> Generator [FileDraft]
genRegister spec =
  return
    [ mkTmplFdWithData
        [relfile|register.ts|]
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

genCrudRegister :: AppSpec -> Generator [FileDraft]
genCrudRegister spec
  | null cruds = return []
  | otherwise =
      return
        [ mkTmplFdWithData
            [relfile|crud-register.ts|]
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

genOperationRegister :: AppSpec -> Generator [FileDraft]
genOperationRegister spec
  | null operations = return []
  | otherwise =
      return
        [ mkTmplFdWithData
            [relfile|operations-register.ts|]
            (object ["operations" .= map mkOperationData operations])
        ]
  where
    operations = getOperations spec

    mkOperationData :: AS.Operation.Operation -> Aeson.Types.Value
    mkOperationData operation =
      object
        [ "jsFn" .= extOperationImportToImportJson (AS.Operation.getFn operation),
          "operationName" .= AS.Operation.getName operation
        ]
