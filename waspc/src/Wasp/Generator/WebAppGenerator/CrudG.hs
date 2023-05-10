module Wasp.Generator.WebAppGenerator.CrudG
  ( genCrud,
  )
where

import Data.Maybe (fromJust)
import StrongPath (reldir, relfile, (</>))
import qualified StrongPath as SP
import Wasp.AppSpec (AppSpec, getCruds)
import qualified Wasp.AppSpec.Crud as AS.Crud
import Wasp.Generator.Crud (getCrudEntityPrimaryField, getCrudOperationJson)
import Wasp.Generator.FileDraft (FileDraft)
import Wasp.Generator.Monad (Generator)
import qualified Wasp.Generator.WebAppGenerator.Common as C

genCrud :: AppSpec -> Generator [FileDraft]
genCrud spec = genCrudOperations spec cruds
  where
    cruds = getCruds spec

genCrudOperations :: AppSpec -> [(String, AS.Crud.Crud)] -> Generator [FileDraft]
genCrudOperations spec cruds = return $ map genCrudOperation cruds
  where
    genCrudOperation :: (String, AS.Crud.Crud) -> FileDraft
    genCrudOperation (name, crud) = C.mkTmplFdWithDstAndData tmplPath destPath (Just tmplData)
      where
        tmplPath = [relfile|src/crud/_crud.ts|]
        destPath = C.webAppSrcDirInWebAppRootDir </> [reldir|crud|] </> fromJust (SP.parseRelFile (name ++ ".ts"))
        tmplData = getCrudOperationJson name crud primaryField
        primaryField = getCrudEntityPrimaryField spec crud
