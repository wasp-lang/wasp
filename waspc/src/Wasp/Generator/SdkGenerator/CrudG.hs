module Wasp.Generator.SdkGenerator.CrudG
  ( genCrud,
  )
where

import Data.Aeson (KeyValue ((.=)), object)
import qualified Data.Aeson.Types as Aeson.Types
import StrongPath (reldir, relfile, (</>))
import Wasp.AppSpec (AppSpec, getCruds)
import qualified Wasp.AppSpec as AS
import qualified Wasp.AppSpec.App as AS.App
import qualified Wasp.AppSpec.App.Auth as AS.Auth
import qualified Wasp.AppSpec.Crud as AS.Crud
import Wasp.AppSpec.Valid (getApp, getIdFieldFromCrudEntity, isAuthEnabled)
import Wasp.Generator.Crud (crudDeclarationToOperationsList, getCrudFilePath, getCrudOperationJson, makeCrudOperationKeyAndJsonPair)
import Wasp.Generator.FileDraft (FileDraft)
import Wasp.Generator.Monad (Generator)
import Wasp.Generator.SdkGenerator.JsImport (extImportToImportJson)
import Wasp.Generator.SdkGenerator.UserCore.Common
  ( mkTmplFdWithDestAndData,
  )

genCrud :: AppSpec -> Generator [FileDraft]
genCrud spec =
  if areThereAnyCruds
    then genCrudServerOperations spec cruds
    else return []
  where
    cruds = getCruds spec
    areThereAnyCruds = not $ null cruds

genCrudServerOperations :: AppSpec -> [(String, AS.Crud.Crud)] -> Generator [FileDraft]
genCrudServerOperations spec cruds = return $ map genCrudOperation cruds
  where
    genCrudOperation :: (String, AS.Crud.Crud) -> FileDraft
    genCrudOperation (name, crud) =
      mkTmplFdWithDestAndData destFile tmplFile (Just tmplData)
      where
        destFile = [reldir|server/crud|] </> getCrudFilePath name "ts"
        tmplFile = [relfile|server/crud/_operationTypes.ts|]
        tmplData =
          object
            [ "crud" .= getCrudOperationJson name crud idField,
              "isAuthEnabled" .= isAuthEnabled spec,
              "userEntityUpper" .= maybeUserEntity,
              "overrides" .= object overrides,
              "queryType" .= queryTsType,
              "actionType" .= actionTsType
            ]
        idField = getIdFieldFromCrudEntity spec crud
        maybeUserEntity = AS.refName . AS.Auth.userEntity <$> maybeAuth
        maybeAuth = AS.App.auth $ snd $ getApp spec

        queryTsType :: String
        queryTsType = if isAuthEnabled spec then "AuthenticatedQueryDefinition" else "UnauthenticatedQueryDefinition"

        actionTsType :: String
        actionTsType = if isAuthEnabled spec then "AuthenticatedActionDefinition" else "UnauthenticatedActionDefinition"

        overrides :: [Aeson.Types.Pair]
        overrides = map operationToOverrideImport crudOperations

        crudOperations = crudDeclarationToOperationsList crud

        operationToOverrideImport :: (AS.Crud.CrudOperation, AS.Crud.CrudOperationOptions) -> Aeson.Types.Pair
        operationToOverrideImport (operation, options) = makeCrudOperationKeyAndJsonPair operation importJson
          where
            importJson = extImportToImportJson $ AS.Crud.overrideFn options
