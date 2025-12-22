module Wasp.Generator.SdkGenerator.Client.CrudG
  ( genNewClientCrudApi,
  )
where

import Data.Aeson (object, (.=))
import qualified Data.Aeson as Aeson
import Data.Maybe (fromJust)
import StrongPath (Dir', File', Path', Rel, Rel', reldir, relfile, (</>))
import qualified StrongPath as SP
import Wasp.AppSpec (AppSpec, getCruds)
import qualified Wasp.AppSpec.Crud as AS.Crud
import Wasp.AppSpec.Valid (getIdFieldFromCrudEntity)
import Wasp.Generator.Crud (getCrudOperationJson)
import Wasp.Generator.FileDraft (FileDraft)
import Wasp.Generator.Monad (Generator)
import Wasp.Generator.SdkGenerator.Client.Common
import Wasp.Generator.SdkGenerator.Common
import Wasp.Util ((<++>))

genNewClientCrudApi :: AppSpec -> Generator [FileDraft]
genNewClientCrudApi spec =
  if areThereAnyCruds
    then
      sequence
        [ genCrudIndex spec cruds,
          genClientCrudFileCopy SdkUserCoreProject [relfile|operationsHelpers.ts|]
        ]
        <++> genCrudOperations spec cruds
    else return []
  where
    cruds = getCruds spec
    areThereAnyCruds = not $ null cruds

genCrudIndex :: AppSpec -> [(String, AS.Crud.Crud)] -> Generator FileDraft
genCrudIndex spec cruds =
  return $
    makeSdkProjectTmplFdWithData SdkUserCoreProject tmplFile tmplData
  where
    tmplFile = clientCrudDirInSdkTemplatesProjectDir </> [relfile|index.ts|]
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
      makeSdkProjectTmplFdWithDestAndData destFile SdkUserCoreProject tmplFile (Just tmplData)
      where
        destFile = [reldir|client/crud|] </> fromJust (SP.parseRelFile (name ++ ".ts"))
        tmplFile = clientCrudDirInSdkTemplatesProjectDir </> [relfile|_crud.ts|]
        tmplData = getCrudOperationJson name crud idField
        idField = getIdFieldFromCrudEntity spec crud

clientCrudDirInSdkTemplatesProjectDir :: Path' (Rel SdkTemplatesProjectDir) Dir'
clientCrudDirInSdkTemplatesProjectDir = clientTemplatesDirInSdkTemplatesDir </> [reldir|crud|]

genClientCrudFileCopy :: SdkProject -> Path' Rel' File' -> Generator FileDraft
genClientCrudFileCopy sdkProject =
  return . makeSdkProjectTmplFd sdkProject . (clientCrudDirInSdkTemplatesProjectDir </>)
