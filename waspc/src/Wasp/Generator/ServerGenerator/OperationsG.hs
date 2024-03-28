{-# LANGUAGE TypeApplications #-}

module Wasp.Generator.ServerGenerator.OperationsG
  ( genOperations,
    queryFileInSrcDir,
    actionFileInSrcDir,
    operationFileInSrcDir,
  )
where

import Data.Aeson (object, (.=))
import qualified Data.Aeson as Aeson
import Data.Maybe (fromJust)
import StrongPath (Dir, File', Path, Path', Posix, Rel, reldir, reldirP, relfile, (</>))
import qualified StrongPath as SP
import Wasp.AppSpec (AppSpec)
import qualified Wasp.AppSpec as AS
import qualified Wasp.AppSpec.Action as AS.Action
import Wasp.AppSpec.Operation (getName)
import qualified Wasp.AppSpec.Operation as AS.Operation
import qualified Wasp.AppSpec.Query as AS.Query
import Wasp.Generator.Common (makeJsonWithEntityData)
import Wasp.Generator.FileDraft (FileDraft)
import Wasp.Generator.Monad (Generator)
import qualified Wasp.Generator.ServerGenerator.Common as C
import Wasp.Generator.ServerGenerator.JsImport (extImportToImportJson)
import Wasp.Util (toUpperFirst, (<++>))

genOperations :: AppSpec -> Generator [FileDraft]
genOperations spec =
  genQueries spec
    <++> genActions spec

genQueries :: AppSpec -> Generator [FileDraft]
genQueries = mapM genQuery . AS.getQueries

genActions :: AppSpec -> Generator [FileDraft]
genActions = mapM genAction . AS.getActions

-- | Here we generate JS file that basically imports JS query function provided by user,
--   decorates it (mostly injects stuff into it) and exports. Idea is that the rest of the server,
--   and user also, should use this new JS function, and not the old one directly.
genQuery :: (String, AS.Query.Query) -> Generator FileDraft
genQuery (queryName, query) = return $ C.mkTmplFdWithDstAndData tmplFile dstFile (Just tmplData)
  where
    operation = AS.Operation.QueryOp queryName query
    tmplFile = C.asTmplFile [relfile|src/queries/_query.ts|]
    dstFile = C.serverSrcDirInServerRootDir </> queryFileInSrcDir queryName
    tmplData = operationTmplData operation

-- | Analogous to genQuery.
genAction :: (String, AS.Action.Action) -> Generator FileDraft
genAction (actionName, action) = return $ C.mkTmplFdWithDstAndData tmplFile dstFile (Just tmplData)
  where
    operation = AS.Operation.ActionOp actionName action
    tmplFile = [relfile|src/actions/_action.ts|]
    dstFile = C.serverSrcDirInServerRootDir </> actionFileInSrcDir actionName
    tmplData = operationTmplData operation

queryFileInSrcDir :: String -> Path' (Rel C.ServerSrcDir) File'
queryFileInSrcDir queryName =
  [reldir|queries|]
    -- TODO: fromJust here could fail if there is some problem with the name, we should handle this.
    </> fromJust (SP.parseRelFile $ queryName ++ ".ts")

actionFileInSrcDir :: String -> Path' (Rel C.ServerSrcDir) File'
actionFileInSrcDir actionName =
  [reldir|actions|]
    -- TODO: fromJust here could fail if there is some problem with the name, we should handle this.
    </> fromJust (SP.parseRelFile $ actionName ++ ".ts")

operationFileInSrcDir :: AS.Operation.Operation -> Path' (Rel C.ServerSrcDir) File'
operationFileInSrcDir (AS.Operation.QueryOp name _) = queryFileInSrcDir name
operationFileInSrcDir (AS.Operation.ActionOp name _) = actionFileInSrcDir name

operationTmplData :: AS.Operation.Operation -> Aeson.Value
operationTmplData operation =
  object
    [ "jsFn" .= extImportToImportJson relPathFromOperationsDirToServerSrcDir (Just $ AS.Operation.getFn operation),
      "operationTypeName" .= toUpperFirst (getName operation),
      "entities"
        .= maybe
          []
          (map (makeJsonWithEntityData . AS.refName))
          (AS.Operation.getEntities operation)
    ]
  where
    relPathFromOperationsDirToServerSrcDir :: Path Posix (Rel importLocation) (Dir C.ServerSrcDir)
    relPathFromOperationsDirToServerSrcDir = [reldirP|../|]
