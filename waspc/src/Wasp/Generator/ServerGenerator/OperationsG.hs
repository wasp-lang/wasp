module Wasp.Generator.ServerGenerator.OperationsG
  ( genOperations,
    queryFileInSrcDir,
    actionFileInSrcDir,
    operationFileInSrcDir,
  )
where

import Data.Aeson (object, (.=))
import qualified Data.Aeson as Aeson
import Data.Char (toLower)
import Data.Maybe (fromJust)
import StrongPath (Dir, Dir', File', Path, Path', Posix, Rel, reldir, reldirP, relfile, (</>))
import qualified StrongPath as SP
import Wasp.Generator.ExternalCodeGenerator.Common (GeneratedExternalCodeDir)
import Wasp.Generator.FileDraft (FileDraft)
import Wasp.Generator.JsImport (getImportDetailsForJsFnImport)
import qualified Wasp.Generator.ServerGenerator.Common as C
import Wasp.Wasp (Wasp)
import qualified Wasp.Wasp as Wasp
import qualified Wasp.Wasp.Action as Wasp.Action
import qualified Wasp.Wasp.Operation as Wasp.Operation
import qualified Wasp.Wasp.Query as Wasp.Query

genOperations :: Wasp -> [FileDraft]
genOperations wasp =
  genQueries wasp
    ++ genActions wasp

genQueries :: Wasp -> [FileDraft]
genQueries wasp =
  map (genQuery wasp) (Wasp.getQueries wasp)

genActions :: Wasp -> [FileDraft]
genActions wasp =
  map (genAction wasp) (Wasp.getActions wasp)

-- | Here we generate JS file that basically imports JS query function provided by user,
--   decorates it (mostly injects stuff into it) and exports. Idea is that the rest of the server,
--   and user also, should use this new JS function, and not the old one directly.
genQuery :: Wasp -> Wasp.Query.Query -> FileDraft
genQuery _ query = C.makeTemplateFD tmplFile dstFile (Just tmplData)
  where
    operation = Wasp.Operation.QueryOp query
    tmplFile = C.asTmplFile [relfile|src/queries/_query.js|]
    dstFile = C.serverSrcDirInServerRootDir </> queryFileInSrcDir query
    tmplData = operationTmplData operation

-- | Analogous to genQuery.
genAction :: Wasp -> Wasp.Action.Action -> FileDraft
genAction _ action = C.makeTemplateFD tmplFile dstFile (Just tmplData)
  where
    operation = Wasp.Operation.ActionOp action
    tmplFile = [relfile|src/actions/_action.js|]
    dstFile = C.serverSrcDirInServerRootDir </> actionFileInSrcDir action
    tmplData = operationTmplData operation

queryFileInSrcDir :: Wasp.Query.Query -> Path' (Rel C.ServerSrcDir) File'
queryFileInSrcDir query =
  [reldir|queries|]
    -- TODO: fromJust here could fail if there is some problem with the name, we should handle this.
    </> fromJust (SP.parseRelFile $ Wasp.Query._name query ++ ".js")

actionFileInSrcDir :: Wasp.Action.Action -> Path' (Rel C.ServerSrcDir) File'
actionFileInSrcDir action =
  [reldir|actions|]
    -- TODO: fromJust here could fail if there is some problem with the name, we should handle this.
    </> fromJust (SP.parseRelFile $ Wasp.Action._name action ++ ".js")

operationFileInSrcDir :: Wasp.Operation.Operation -> Path' (Rel C.ServerSrcDir) File'
operationFileInSrcDir (Wasp.Operation.QueryOp query) = queryFileInSrcDir query
operationFileInSrcDir (Wasp.Operation.ActionOp action) = actionFileInSrcDir action

-- | TODO: Make this not hardcoded!
relPosixPathFromOperationFileToExtSrcDir :: Path Posix (Rel Dir') (Dir GeneratedExternalCodeDir)
relPosixPathFromOperationFileToExtSrcDir = [reldirP|../ext-src/|]

operationTmplData :: Wasp.Operation.Operation -> Aeson.Value
operationTmplData operation =
  object
    [ "jsFnImportStatement" .= importStmt,
      "jsFnIdentifier" .= importIdentifier,
      "entities" .= maybe [] (map buildEntityData) (Wasp.Operation.getEntities operation)
    ]
  where
    (importIdentifier, importStmt) =
      getImportDetailsForJsFnImport relPosixPathFromOperationFileToExtSrcDir $
        Wasp.Operation.getJsFn operation
    buildEntityData :: String -> Aeson.Value
    buildEntityData entityName =
      object
        [ "name" .= entityName,
          "prismaIdentifier" .= (toLower (head entityName) : tail entityName)
        ]
