module Wasp.Generator.SdkGenerator.CrudG
  ( genCrud,
  )
where

import Data.Aeson (KeyValue ((.=)), object)
import StrongPath
  ( reldir,
    relfile,
    (</>),
  )
import Wasp.AppSpec (AppSpec, getCruds)
import qualified Wasp.AppSpec.Crud as AS.Crud
import Wasp.AppSpec.Valid (getIdFieldFromCrudEntity, isAuthEnabled)
import Wasp.Generator.Crud (getCrudFilePath, getCrudOperationJson)
import Wasp.Generator.FileDraft (FileDraft)
import Wasp.Generator.Monad (Generator)
import Wasp.Generator.SdkGenerator.Common (mkTmplFdWithDstAndData)

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
    genCrudOperation (name, crud) =
      mkTmplFdWithDstAndData
        [relfile|server/crud/_operationTypes.ts|]
        ([reldir|server/crud|] </> getCrudFilePath name "ts")
        (Just tmplData)
      where
        tmplData =
          object
            [ "crud" .= getCrudOperationJson name crud idField,
              "isAuthEnabled" .= isAuthEnabled spec,
              "queryType" .= queryTsType,
              "actionType" .= actionTsType
            ]
        idField = getIdFieldFromCrudEntity spec crud

        queryTsType :: String
        queryTsType = if isAuthEnabled spec then "AuthenticatedQueryDefinition" else "UnauthenticatedQueryDefinition"

        actionTsType :: String
        actionTsType = if isAuthEnabled spec then "AuthenticatedActionDefinition" else "UnauthenticatedActionDefinition"
