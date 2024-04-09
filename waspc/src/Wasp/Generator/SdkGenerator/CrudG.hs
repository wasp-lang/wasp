module Wasp.Generator.SdkGenerator.CrudG
  ( genCrud,
  )
where

import Data.Aeson (KeyValue ((.=)), object)
import qualified Data.Aeson.Types as Aeson.Types
import StrongPath
  ( reldir,
    relfile,
    (</>),
  )
import Wasp.AppSpec (AppSpec, getCruds)
import qualified Wasp.AppSpec as AS
import qualified Wasp.AppSpec.App as AS.App
import qualified Wasp.AppSpec.App.Auth as AS.Auth
import qualified Wasp.AppSpec.Crud as AS.Crud
import Wasp.AppSpec.Valid (getApp, getIdFieldFromCrudEntity, isAuthEnabled)
import Wasp.Generator.Crud (crudDeclarationToOperationsList, getCrudFilePath, getCrudOperationJson, makeCrudOperationKeyAndJsonPair)
import Wasp.Generator.FileDraft (FileDraft)
import Wasp.Generator.Monad (Generator)
import Wasp.Generator.SdkGenerator.Common (extImportToSdkImportJson)
import qualified Wasp.Generator.SdkGenerator.Common as C

genCrud :: AppSpec -> Generator [FileDraft]
genCrud spec =
  if areThereAnyCruds
    then genCrudServerOperations spec cruds
    else return []
  where
    cruds = getCruds spec
    areThereAnyCruds = not $ null cruds

genCrudServerOperations :: AppSpec -> [(String, AS.Crud.Crud)] -> Generator [FileDraft]
genCrudServerOperations spec cruds = mapM genCrudOperation cruds
  where
    genCrudOperation :: (String, AS.Crud.Crud) -> Generator FileDraft
    genCrudOperation (name, crud) = do
      overrides <- mapM operationToOverrideImport crudOperations

      let tmplData =
            object
              [ "crud" .= getCrudOperationJson name crud idField,
                "isAuthEnabled" .= isAuthEnabled spec,
                "userEntityUpper" .= maybeUserEntity,
                "overrides" .= object overrides,
                "queryType" .= queryTsType,
                "actionType" .= actionTsType
              ]

      return $ C.mkTmplFdWithDstAndData tmplPath destPath (Just tmplData)
      where
        tmplPath = [relfile|server/crud/_operationTypes.ts|]
        destPath = [reldir|server/crud|] </> getCrudFilePath name "ts"
        idField = getIdFieldFromCrudEntity spec crud
        maybeUserEntity = AS.refName . AS.Auth.userEntity <$> maybeAuth
        maybeAuth = AS.App.auth $ snd $ getApp spec

        queryTsType :: String
        queryTsType = if isAuthEnabled spec then "AuthenticatedQuery" else "Query"

        actionTsType :: String
        actionTsType = if isAuthEnabled spec then "AuthenticatedAction" else "Action"

        crudOperations = crudDeclarationToOperationsList crud

        operationToOverrideImport :: (AS.Crud.CrudOperation, AS.Crud.CrudOperationOptions) -> Generator Aeson.Types.Pair
        operationToOverrideImport (operation, options) = do
          importJson <- extImportToSdkImportJson $ AS.Crud.overrideFn options
          return $ makeCrudOperationKeyAndJsonPair operation importJson
