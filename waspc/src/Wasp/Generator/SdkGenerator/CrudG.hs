module Wasp.Generator.SdkGenerator.CrudG
  ( genCrud,
  )
where

import Data.Aeson (KeyValue ((.=)), object)
import qualified Data.Aeson.Types as Aeson.Types
import Data.Maybe (fromJust)
import StrongPath
  ( reldir,
    reldirP,
    relfile,
    (</>),
  )
import qualified StrongPath as SP
import Wasp.AppSpec (AppSpec, getCruds)
import qualified Wasp.AppSpec as AS
import qualified Wasp.AppSpec.App as AS.App
import qualified Wasp.AppSpec.App.Auth as AS.Auth
import qualified Wasp.AppSpec.Crud as AS.Crud
import Wasp.AppSpec.Valid (getApp, getIdFieldFromCrudEntity, isAuthEnabled)
import Wasp.Generator.Crud (crudDeclarationToOperationsList, getCrudFilePath, getCrudOperationJson, makeCrudOperationKeyAndJsonPair)
import Wasp.Generator.FileDraft (FileDraft)
import qualified Wasp.Generator.JsImport as GJI
import Wasp.Generator.Monad (Generator)
import Wasp.Generator.SdkGenerator.Common (extImportToJsImport)
import qualified Wasp.Generator.SdkGenerator.Common as C
import qualified Wasp.Util.StrongPath as SP

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
    genCrudOperation (name, crud) = C.mkTmplFdWithDstAndData tmplPath destPath (Just tmplData)
      where
        tmplPath = crudDirInSdkRootDir </> [relfile|_operationTypes.ts|]
        destPath = crudDirInSdkRootDir </> getCrudFilePath name "ts"
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
            importJson = GJI.jsImportToImportJson $ extImportToJsImport sdkRootDirFromCrudDir <$> AS.Crud.overrideFn options
        crudDirInSdkRootDir = [reldir|server/crud|]
        sdkRootDirFromCrudDir = SP.reversePosixPath $ fromJust $ SP.relDirToPosix crudDirInSdkRootDir
