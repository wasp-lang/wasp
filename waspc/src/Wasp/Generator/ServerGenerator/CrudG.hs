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
import Wasp.AppSpec.Valid (getApp, isAuthEnabled)
import Wasp.Generator.Crud
  ( crudDeclarationToOperationsList,
    getCrudEntityPrimaryField,
    getCrudFilePath,
    getCrudOperationJson,
    makeCrudOperationKeyAndJsonPair,
  )
import qualified Wasp.Generator.Crud.Routes as Routes
import Wasp.Generator.FileDraft (FileDraft)
import Wasp.Generator.Monad (Generator)
import qualified Wasp.Generator.ServerGenerator.Common as C
import Wasp.Generator.ServerGenerator.JsImport (extImportToImportJson)
import qualified Wasp.JsImport as JI
import Wasp.Util ((<++>))

genCrud :: AppSpec -> Generator [FileDraft]
genCrud spec =
  if areThereAnyCruds
    then
      sequence
        [ genCrudIndexRoute spec cruds
        ]
        <++> genCrudRoutes spec cruds
        <++> genCrudOperations spec cruds
    else return []
  where
    cruds = AS.getCruds spec
    areThereAnyCruds = not . null $ cruds

genCrudIndexRoute :: AppSpec -> [(String, AS.Crud.Crud)] -> Generator FileDraft
genCrudIndexRoute spec cruds = return $ C.mkTmplFdWithData tmplPath (Just tmplData)
  where
    tmplPath = [relfile|src/routes/crud/index.ts|]
    tmplData =
      object
        [ "crudRouters" .= map getCrudRouterData cruds,
          "isAuthEnabled" .= isAuthEnabled spec
        ]

    getCrudRouterData :: (String, AS.Crud.Crud) -> Data.Aeson.Value
    getCrudRouterData (name, _) =
      object
        [ "importStatement" .= importStatement,
          "importIdentifier" .= importIdentifier,
          "route" .= Routes.getCrudOperationRouterRoute name
        ]
      where
        (importStatement, importIdentifier) =
          JI.getJsImportStmtAndIdentifier
            JI.JsImport
              { JI._name = JI.JsImportField name,
                JI._path = fromJust . SP.relFileToPosix $ getCrudFilePath name "js",
                JI._importAlias = Nothing
              }

genCrudRoutes :: AppSpec -> [(String, AS.Crud.Crud)] -> Generator [FileDraft]
genCrudRoutes spec cruds = return $ map genCrudRoute cruds
  where
    genCrudRoute :: (String, AS.Crud.Crud) -> FileDraft
    genCrudRoute (name, crud) = C.mkTmplFdWithDstAndData tmplPath destPath (Just tmplData)
      where
        tmplPath = [relfile|src/routes/crud/_crud.ts|]
        destPath = C.serverSrcDirInServerRootDir </> [reldir|routes/crud|] </> getCrudFilePath name "ts"
        tmplData =
          object
            [ "crud" .= getCrudOperationJson name crud primaryField,
              "isAuthEnabled" .= isAuthEnabled spec
            ]
        -- We validated in analyzer that entity field exists, so we can safely use fromJust here.
        primaryField = fromJust $ getCrudEntityPrimaryField spec crud

genCrudOperations :: AppSpec -> [(String, AS.Crud.Crud)] -> Generator [FileDraft]
genCrudOperations spec cruds = return $ map genCrudOperation cruds
  where
    genCrudOperation :: (String, AS.Crud.Crud) -> FileDraft
    genCrudOperation (name, crud) = C.mkTmplFdWithDstAndData tmplPath destPath (Just tmplData)
      where
        tmplPath = [relfile|src/crud/_operations.ts|]
        destPath = C.serverSrcDirInServerRootDir </> [reldir|crud|] </> getCrudFilePath name "ts"
        tmplData =
          object
            [ "crud" .= getCrudOperationJson name crud primaryField,
              "isAuthEnabled" .= isAuthEnabled spec,
              "userEntityUpper" .= maybeUserEntity,
              "overrides" .= object overrides
            ]
        -- We validated in analyzer that entity field exists, so we can safely use fromJust here.
        primaryField = fromJust $ getCrudEntityPrimaryField spec crud
        maybeUserEntity = AS.refName . AS.Auth.userEntity <$> maybeAuth
        maybeAuth = AS.App.auth $ snd $ getApp spec

        overrides :: [Aeson.Types.Pair]
        overrides = map operationToOverrideImport crudOperations

        crudOperations = crudDeclarationToOperationsList crud

        operationToOverrideImport :: (AS.Crud.CrudOperation, AS.Crud.CrudOperationOptions) -> Aeson.Types.Pair
        operationToOverrideImport (operation, options) = makeCrudOperationKeyAndJsonPair operation importJson
          where
            importJson = extImportToImportJson [reldirP|../|] (AS.Crud.overrideFn options)
