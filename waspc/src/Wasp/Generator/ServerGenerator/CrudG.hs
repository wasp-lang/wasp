module Wasp.Generator.ServerGenerator.CrudG
  ( genCrud,
  )
where

import Data.Aeson (object, (.=))
import qualified Data.Aeson
import Data.Maybe (fromJust)
import StrongPath (File', Path', Rel, reldir, relfile, (</>))
import qualified StrongPath as SP
import Wasp.AppSpec (AppSpec, getCruds)
import qualified Wasp.AppSpec.Crud as AS.Crud
import Wasp.AppSpec.Valid (isAuthEnabled)
import Wasp.Generator.Crud (getCrudEntityPrimaryField, getCrudOperationJson)
import qualified Wasp.Generator.Crud.Routes as Routes
import Wasp.Generator.FileDraft (FileDraft)
import Wasp.Generator.Monad (Generator)
import qualified Wasp.Generator.ServerGenerator.Common as C
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
    else return []
  where
    cruds = getCruds spec
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

getCrudFilePath :: String -> String -> Path' (Rel r) File'
getCrudFilePath crudName ext = fromJust (SP.parseRelFile (crudName ++ "." ++ ext))
