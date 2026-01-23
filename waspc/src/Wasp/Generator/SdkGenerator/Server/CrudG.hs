module Wasp.Generator.SdkGenerator.Server.CrudG
  ( genNewServerCrudApi,
  )
where

import Data.Aeson (object, (.=))
import qualified Data.Aeson as Aeson
import StrongPath (Dir', Path', Rel, reldir, relfile, (</>))
import Wasp.AppSpec (AppSpec, getCruds)
import qualified Wasp.AppSpec.Crud as AS.Crud
import Wasp.AppSpec.Valid (getIdFieldFromCrudEntity)
import Wasp.Generator.Crud (getCrudOperationJson)
import Wasp.Generator.FileDraft (FileDraft)
import Wasp.Generator.Monad (Generator)
import Wasp.Generator.SdkGenerator.Common
  ( SdkTemplatesDir,
    mkTmplFdWithData,
  )

genNewServerCrudApi :: AppSpec -> Generator [FileDraft]
genNewServerCrudApi spec =
  if areThereAnyCruds
    then sequence [genCrudIndex spec cruds]
    else return []
  where
    cruds = getCruds spec
    areThereAnyCruds = not $ null cruds

genCrudIndex :: AppSpec -> [(String, AS.Crud.Crud)] -> Generator FileDraft
genCrudIndex spec cruds =
  return $ mkTmplFdWithData tmplFile tmplData
  where
    tmplFile = serverCrudDirInSdkTemplatesDir </> [relfile|index.ts|]
    tmplData = object ["cruds" .= map getCrudOperationJsonFromCrud cruds]
    getCrudOperationJsonFromCrud :: (String, AS.Crud.Crud) -> Aeson.Value
    getCrudOperationJsonFromCrud (name, crud) = getCrudOperationJson name crud idField
      where
        idField = getIdFieldFromCrudEntity spec crud

serverCrudDirInSdkTemplatesDir :: Path' (Rel SdkTemplatesDir) Dir'
serverCrudDirInSdkTemplatesDir = [reldir|server/crud|]
