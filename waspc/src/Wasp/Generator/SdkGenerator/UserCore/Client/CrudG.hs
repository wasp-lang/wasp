module Wasp.Generator.SdkGenerator.UserCore.Client.CrudG
  ( genNewClientCrudApi,
  )
where

import Data.Aeson (object, (.=))
import qualified Data.Aeson as Aeson
import Data.Maybe (fromJust)
import StrongPath (parseRelFile, reldir, relfile, (</>))
import Wasp.AppSpec (AppSpec, getCruds)
import qualified Wasp.AppSpec.Crud as AS.Crud
import Wasp.AppSpec.Valid (getIdFieldFromCrudEntity)
import Wasp.Generator.Crud (getCrudOperationJson)
import Wasp.Generator.FileDraft (FileDraft)
import Wasp.Generator.Monad (Generator)
import Wasp.Generator.SdkGenerator.UserCore.Common
  ( mkTmplFd,
    mkTmplFdWithData,
    mkTmplFdWithDstAndData,
  )
import Wasp.Util ((<++>))

genNewClientCrudApi :: AppSpec -> Generator [FileDraft]
genNewClientCrudApi spec =
  if areThereAnyCruds
    then
      sequence
        [ genCrudIndex spec cruds,
          return . mkTmplFd $ [relfile|client/crud/operationsHelpers.ts|]
        ]
        <++> genCrudOperations spec cruds
    else return []
  where
    cruds = getCruds spec
    areThereAnyCruds = not $ null cruds

genCrudIndex :: AppSpec -> [(String, AS.Crud.Crud)] -> Generator FileDraft
genCrudIndex spec cruds =
  return $ mkTmplFdWithData [relfile|client/crud/index.ts|] tmplData
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
    genCrudOperation (name, crud) =
      mkTmplFdWithDstAndData
        [relfile|client/crud/_crud.ts|]
        ([reldir|client/crud|] </> fromJust (parseRelFile (name ++ ".ts")))
        (Just tmplData)
      where
        tmplData = getCrudOperationJson name crud idField
        idField = getIdFieldFromCrudEntity spec crud
