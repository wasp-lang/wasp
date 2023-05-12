module Wasp.Generator.ServerGenerator.CrudG
  ( genCrud,
  )
where

import Data.Aeson (object, (.=))
import qualified Data.Aeson
import Data.Maybe (fromJust)
import StrongPath (reldir, reldirP, relfile, (</>))
import qualified StrongPath as SP
import Wasp.AppSpec (AppSpec)
import qualified Wasp.AppSpec as AS
import qualified Wasp.AppSpec.App as AS.App
import qualified Wasp.AppSpec.App.Auth as AS.Auth
import qualified Wasp.AppSpec.Crud as AS.Crud
import Wasp.AppSpec.Valid (getApp, isAuthEnabled)
import Wasp.Generator.Crud (getCrudEntityPrimaryField, getCrudFilePath, getCrudOperationJson)
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
        <++> genCrudHelpers spec cruds
        <++> genCrudsTypes spec cruds
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
              "overrides" .= extImportToImportJson [reldirP|../../|] (AS.Crud.overrides crud),
              "isAuthEnabled" .= isAuthEnabled spec,
              "prismaArgs" .= object ["importStatement" .= prismaArgsImportStatement, "importIdentifier" .= prismaArgsImportIdentifier],
              "routeInputs" .= object ["importStatement" .= routeInputsImportStatement, "importIdentifier" .= routeInputsImportIdentifier]
            ]
        -- We validated in analyzer that entity field exists, so we can safely use fromJust here.
        primaryField = fromJust $ getCrudEntityPrimaryField spec crud
        (prismaArgsImportStatement, prismaArgsImportIdentifier) =
          JI.getJsImportStmtAndIdentifier
            JI.JsImport
              { JI._name = JI.JsImportField "PrismaArgs",
                JI._path = crudHelpersFilePath,
                JI._importAlias = Nothing
              }
        (routeInputsImportStatement, routeInputsImportIdentifier) =
          JI.getJsImportStmtAndIdentifier
            JI.JsImport
              { JI._name = JI.JsImportField "RouteInputs",
                JI._path = crudHelpersFilePath,
                JI._importAlias = Nothing
              }
        crudHelpersFilePath = [reldirP|../../crud|] </> (fromJust . SP.relFileToPosix $ getCrudFilePath name "js")

genCrudHelpers :: AppSpec -> [(String, AS.Crud.Crud)] -> Generator [FileDraft]
genCrudHelpers spec cruds = return $ map genCrudRoute cruds
  where
    genCrudRoute :: (String, AS.Crud.Crud) -> FileDraft
    genCrudRoute (name, crud) = C.mkTmplFdWithDstAndData tmplPath destPath (Just tmplData)
      where
        tmplPath = [relfile|src/crud/_helpers.ts|]
        destPath = C.serverSrcDirInServerRootDir </> [reldir|crud|] </> getCrudFilePath name "ts"
        tmplData =
          object
            [ "crud" .= getCrudOperationJson name crud primaryField,
              "isAuthEnabled" .= isAuthEnabled spec,
              "userEntityUpper" .= maybeUserEntity
            ]
        -- We validated in analyzer that entity field exists, so we can safely use fromJust here.
        primaryField = fromJust $ getCrudEntityPrimaryField spec crud
        maybeUserEntity = AS.refName . AS.Auth.userEntity <$> maybeAuth
        maybeAuth = AS.App.auth $ snd $ getApp spec

genCrudsTypes :: AppSpec -> [(String, AS.Crud.Crud)] -> Generator [FileDraft]
genCrudsTypes spec cruds = return $ map genCrudTypes cruds
  where
    genCrudTypes :: (String, AS.Crud.Crud) -> FileDraft
    genCrudTypes (name, crud) = C.mkUniversalTmplFdWithDstAndData tmplPath destPath (Just tmplData)
      where
        tmplPath = [relfile|_crudTypes.ts|]
        destPath = C.serverSrcDirInServerRootDir </> [reldir|universal/crud|] </> getCrudFilePath name "ts"
        tmplData =
          object
            [ "crud" .= getCrudOperationJson name crud primaryField
            ]
        -- We validated in analyzer that entity field exists, so we can safely use fromJust here.
        primaryField = fromJust $ getCrudEntityPrimaryField spec crud
