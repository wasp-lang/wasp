module Wasp.Generator.WebAppGenerator.CrudG
  ( genCrud,
  )
where

import Data.Aeson (KeyValue ((.=)), object)
import Data.Maybe (fromJust)
import StrongPath (reldir, relfile, (</>))
import qualified StrongPath as SP
import Wasp.AppSpec (AppSpec, getCruds)
import qualified Wasp.AppSpec.Crud as AS.Crud
import Wasp.Generator.Crud (getCrudEntityPrimaryField, getCrudFilePath, getCrudOperationJson)
import Wasp.Generator.FileDraft (FileDraft)
import Wasp.Generator.Monad (Generator)
import qualified Wasp.Generator.WebAppGenerator.Common as C
import Wasp.Util ((<++>))

genCrud :: AppSpec -> Generator [FileDraft]
genCrud spec =
  if areThereAnyCruds
    then
      genCrudOperations spec cruds
        <++> genCrudsTypes spec cruds
    else return []
  where
    cruds = getCruds spec
    areThereAnyCruds = not $ null cruds

genCrudOperations :: AppSpec -> [(String, AS.Crud.Crud)] -> Generator [FileDraft]
genCrudOperations spec cruds = return $ map genCrudOperation cruds
  where
    genCrudOperation :: (String, AS.Crud.Crud) -> FileDraft
    genCrudOperation (name, crud) = C.mkTmplFdWithDstAndData tmplPath destPath (Just tmplData)
      where
        tmplPath = [relfile|src/crud/_crud.ts|]
        destPath = C.webAppSrcDirInWebAppRootDir </> [reldir|crud|] </> fromJust (SP.parseRelFile (name ++ ".ts"))
        tmplData = getCrudOperationJson name crud primaryField
        -- We validated in analyzer that entity field exists, so we can safely use fromJust here.
        primaryField = fromJust $ getCrudEntityPrimaryField spec crud

genCrudsTypes :: AppSpec -> [(String, AS.Crud.Crud)] -> Generator [FileDraft]
genCrudsTypes spec cruds = return $ map genCrudTypes cruds
  where
    genCrudTypes :: (String, AS.Crud.Crud) -> FileDraft
    genCrudTypes (name, crud) = C.mkUniversalTmplFdWithDstAndData tmplPath destPath (Just tmplData)
      where
        tmplPath = [relfile|_crudTypes.ts|]
        destPath = C.webAppSrcDirInWebAppRootDir </> [reldir|universal/crud|] </> getCrudFilePath name "ts"
        tmplData =
          object
            [ "crud" .= getCrudOperationJson name crud primaryField
            ]
        -- We validated in analyzer that entity field exists, so we can safely use fromJust here.
        primaryField = fromJust $ getCrudEntityPrimaryField spec crud
