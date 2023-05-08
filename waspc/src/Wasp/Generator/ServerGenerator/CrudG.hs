module Wasp.Generator.ServerGenerator.CrudG
  ( genCrud,
  )
where

import Data.Aeson (object, (.=))
import qualified Data.Aeson
import Data.Maybe (fromJust)
import StrongPath (reldir, relfile, (</>))
import qualified StrongPath as SP
import Wasp.AppSpec (AppSpec, getCruds)
import qualified Wasp.AppSpec.Crud as AS.Crud
import Wasp.Generator.Crud (getCrudOperationJson)
import Wasp.Generator.FileDraft (FileDraft)
import Wasp.Generator.Monad (Generator)
import qualified Wasp.Generator.ServerGenerator.Common as C
import Wasp.Util ((<++>))

genCrud :: AppSpec -> Generator [FileDraft]
genCrud spec =
  sequence
    [ genCrudIndexRoute cruds
    ]
    <++> genCrudRoutes cruds
  where
    cruds = getCruds spec

genCrudIndexRoute :: [(String, AS.Crud.Crud)] -> Generator FileDraft
genCrudIndexRoute cruds = return $ C.mkTmplFdWithData tmplPath (Just tmplData)
  where
    tmplPath = [relfile|src/routes/crud/index.ts|]
    tmplData = object ["crudRouters" .= map getCrudRouterData cruds]

    getCrudRouterData :: (String, AS.Crud.Crud) -> Data.Aeson.Value
    getCrudRouterData (name, crud) =
      object
        [ "importStatement" .= ("import { " ++ name ++ " } from \"./" ++ name ++ ".js\";"),
          "routePath" .= ("/" ++ name),
          "importIdentifier" .= name
        ]

genCrudRoutes :: [(String, AS.Crud.Crud)] -> Generator [FileDraft]
genCrudRoutes cruds = return $ map genCrudRoute cruds
  where
    genCrudRoute :: (String, AS.Crud.Crud) -> FileDraft
    genCrudRoute (name, crud) = C.mkTmplFdWithDstAndData tmplPath destPath (Just tmplData)
      where
        tmplPath = [relfile|src/routes/crud/_crud.ts|]
        destPath = C.serverSrcDirInServerRootDir </> [reldir|routes/crud|] </> fromJust (SP.parseRelFile (name ++ ".ts"))
        tmplData = getCrudOperationJson name crud
