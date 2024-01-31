module Wasp.Generator.SdkGenerator.Client.CrudG
  ( genNewClientCrudApi,
  )
where

import Data.Aeson (object, (.=))
import qualified Data.Aeson as Aeson
import Data.Maybe (fromJust)
import StrongPath (reldir, relfile, (</>))
import qualified StrongPath as SP
import Wasp.AppSpec (AppSpec, getCruds)
import qualified Wasp.AppSpec.Crud as AS.Crud
import Wasp.AppSpec.Valid (getIdFieldFromCrudEntity)
import Wasp.Generator.Crud (getCrudOperationJson)
import Wasp.Generator.FileDraft (FileDraft)
import Wasp.Generator.Monad (Generator)
import qualified Wasp.Generator.SdkGenerator.Common as C
import Wasp.Util ((<++>))

genNewClientCrudApi :: AppSpec -> Generator [FileDraft]
genNewClientCrudApi spec =
  if areThereAnyCruds
    then
      sequence
        [ genCrudIndex spec cruds
        ]
        <++> genCrudOperations spec cruds
    else return []
  where
    cruds = getCruds spec
    areThereAnyCruds = not $ null cruds

genCrudIndex :: AppSpec -> [(String, AS.Crud.Crud)] -> Generator FileDraft
genCrudIndex spec cruds = return $ C.mkTmplFdWithData [relfile|client/crud/index.ts|] tmplData
  where
    tmplData = object ["cruds" .= map getCrudOperationJsonFromCrud cruds]
    getCrudOperationJsonFromCrud :: (String, AS.Crud.Crud) -> Aeson.Value
    getCrudOperationJsonFromCrud (name, crud) = getCrudOperationJson name crud idField
      where
        idField = getIdFieldFromCrudEntity spec crud

genCrudOperations :: AppSpec -> [(String, AS.Crud.Crud)] -> Generator [FileDraft]
genCrudOperations spec cruds = return $ map genCrudOperation cruds
  where
    genCrudOperation :: (String, AS.Crud.Crud) -> FileDraft
    genCrudOperation (name, crud) = C.mkTmplFdWithDstAndData tmplPath destPath (Just tmplData)
      where
        tmplPath = [relfile|client/crud/_crud.ts|]
        destPath = [reldir|client/crud|] </> fromJust (SP.parseRelFile (name ++ ".ts"))
        tmplData = getCrudOperationJson name crud idField
        idField = getIdFieldFromCrudEntity spec crud
