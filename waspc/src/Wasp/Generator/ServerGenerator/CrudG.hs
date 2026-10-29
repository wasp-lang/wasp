module Wasp.Generator.ServerGenerator.CrudG
  ( genCrud,
  )
where

import Data.Aeson (object, (.=))
import qualified Data.Aeson
import qualified Data.Aeson.Types as Aeson.Types
import Data.Maybe (fromJust)
import StrongPath (reldir, reldirP, relfile, (</>))
import qualified StrongPath as SP
import Wasp.AppSpec (AppSpec)
import qualified Wasp.AppSpec as AS
import qualified Wasp.AppSpec.App as AS.App
import qualified Wasp.AppSpec.App.Auth as AS.Auth
import qualified Wasp.AppSpec.Crud as AS.Crud
import Wasp.AppSpec.Valid (getApp, getIdFieldFromCrudEntity, isAuthEnabled)
import Wasp.Generator.Crud
  ( getCrudFilePath,
    getCrudOperationJson,
    makeCrudOperationKeyAndJsonPair,
  )
import qualified Wasp.Generator.Crud.Routes as Routes
import Wasp.Generator.FileDraft (FileDraft)
import Wasp.Generator.Monad (Generator)
import qualified Wasp.Generator.ServerGenerator.Common as C
import Wasp.Generator.ServerGenerator.JsImport (extImportToImportJson)
import Wasp.JsImport (JsImportPath (RelativeImportPath))
import qualified Wasp.JsImport as JI
import Wasp.Util ((<++>))

genCrud :: AppSpec -> Generator [FileDraft]
genCrud spec =
  if areThereAnyCruds
    then
      sequence [genCrudIndexRoute cruds]
        <++> genCrudRoutes spec cruds
        <++> genCrudOperations spec cruds
    else return []
  where
    cruds = AS.getCruds spec
    areThereAnyCruds = not . null $ cruds

genCrudIndexRoute :: [AS.Crud.Crud] -> Generator FileDraft
genCrudIndexRoute cruds = return $ C.mkTmplFdWithData tmplPath (Just tmplData)
  where
    tmplPath = [relfile|src/routes/crud/index.ts|]
    tmplData = object ["crudRouters" .= map getCrudRouterData cruds]

    getCrudRouterData :: AS.Crud.Crud -> Data.Aeson.Value
    getCrudRouterData crud =
      object
        [ "importStatement" .= importStatement,
          "importIdentifier" .= importIdentifier,
          "route" .= Routes.getCrudOperationRouterRoute crud.name
        ]
      where
        (importStatement, importIdentifier) =
          JI.getJsImportStmtAndIdentifier
            JI.JsImport
              { JI._kind = JI.ValueImport,
                JI._name = JI.JsImportField crud.name,
                JI._path = RelativeImportPath (fromJust . SP.relFileToPosix $ getCrudFilePath crud.name "js"),
                JI._importAlias = Nothing
              }

genCrudRoutes :: AppSpec -> [AS.Crud.Crud] -> Generator [FileDraft]
genCrudRoutes spec cruds = return $ map genCrudRoute cruds
  where
    genCrudRoute :: AS.Crud.Crud -> FileDraft
    genCrudRoute crud = C.mkTmplFdWithDstAndData tmplPath destPath (Just tmplData)
      where
        tmplPath = [relfile|src/routes/crud/_crud.ts|]
        destPath = C.serverSrcDirInServerRootDir </> [reldir|routes/crud|] </> getCrudFilePath crud.name "ts"
        tmplData =
          object
            [ "crud" .= getCrudOperationJson crud idField,
              "isAuthEnabled" .= isAuthEnabled spec
            ]
        -- Analyzer ensures that the entity field exists, so fromJust is safe here.
        idField = getIdFieldFromCrudEntity spec crud

genCrudOperations :: AppSpec -> [AS.Crud.Crud] -> Generator [FileDraft]
genCrudOperations spec cruds = return $ map genCrudOperation cruds
  where
    genCrudOperation :: AS.Crud.Crud -> FileDraft
    genCrudOperation crud = C.mkTmplFdWithDstAndData tmplPath destPath (Just tmplData)
      where
        tmplPath = [relfile|src/crud/_operations.ts|]
        destPath = C.serverSrcDirInServerRootDir </> [reldir|crud|] </> getCrudFilePath crud.name "ts"
        tmplData =
          object
            [ "crud" .= getCrudOperationJson crud idField,
              "isAuthEnabled" .= isAuthEnabled spec,
              "userEntityUpper" .= maybeUserEntity,
              "overrides" .= object overrides,
              "queryType" .= queryTsType,
              "actionType" .= actionTsType
            ]
        idField = getIdFieldFromCrudEntity spec crud
        maybeUserEntity = AS.refName . AS.Auth.userEntity <$> maybeAuth
        maybeAuth = AS.App.auth $ getApp spec

        queryTsType :: String
        queryTsType = if isAuthEnabled spec then "AuthenticatedQueryDefinition" else "UnauthenticatedQueryDefinition"

        actionTsType :: String
        actionTsType = if isAuthEnabled spec then "AuthenticatedActionDefinition" else "UnauthenticatedActionDefinition"

        overrides :: [Aeson.Types.Pair]
        overrides = map operationToOverrideImport crudOperations

        crudOperations = AS.Crud.toOperationList crud.operations

        operationToOverrideImport :: (AS.Crud.CrudOperation, AS.Crud.CrudOperationOptions) -> Aeson.Types.Pair
        operationToOverrideImport (operation, options) = makeCrudOperationKeyAndJsonPair operation importJson
          where
            importJson = extImportToImportJson [reldirP|../|] (AS.Crud.overrideFn options)
