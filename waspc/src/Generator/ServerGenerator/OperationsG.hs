module Generator.ServerGenerator.OperationsG
  ( genOperations,
    queryFileInSrcDir,
    actionFileInSrcDir,
    operationFileInSrcDir,
  )
where

import Data.Aeson (object, (.=))
import qualified Data.Aeson as Aeson
import Data.Char (toLower)
import Data.Maybe (fromJust, fromMaybe)
import Generator.FileDraft (FileDraft)
import qualified Generator.ServerGenerator.Common as C
import StrongPath (File', Path', Rel, reldir, relfile, (</>))
import qualified StrongPath as SP
import Wasp (Wasp)
import qualified Wasp
import qualified Wasp.Action
import qualified Wasp.JsImport
import qualified Wasp.Operation
import qualified Wasp.Query

genOperations :: Wasp -> [FileDraft]
genOperations wasp =
  concat
    [ genQueries wasp,
      genActions wasp
    ]

genQueries :: Wasp -> [FileDraft]
genQueries wasp =
  concat
    [ map (genQuery wasp) (Wasp.getQueries wasp)
    ]

genActions :: Wasp -> [FileDraft]
genActions wasp =
  concat
    [ map (genAction wasp) (Wasp.getActions wasp)
    ]

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

-- | TODO: Make this not hardcoded! Maybe even use StrongPath? But I can't because of "../" .
relPosixPathFromOperationFileToExtSrcDir :: FilePath -- Posix
relPosixPathFromOperationFileToExtSrcDir = "../ext-src/"

operationTmplData :: Wasp.Operation.Operation -> Aeson.Value
operationTmplData operation =
  object
    [ "jsFnImportStatement" .= importStmt,
      "jsFnIdentifier" .= importIdentifier,
      "entities" .= map buildEntityData (fromMaybe [] $ Wasp.Operation.getEntities operation)
    ]
  where
    (importIdentifier, importStmt) =
      getImportDetailsForOperationUserJsFn operation relPosixPathFromOperationFileToExtSrcDir
    buildEntityData :: String -> Aeson.Value
    buildEntityData entityName =
      object
        [ "name" .= entityName,
          "prismaIdentifier" .= (toLower (head entityName) : tail entityName)
        ]

-- | Given Wasp operation, it returns details on how to import its user js function and use it,
--   "user js function" meaning the one provided by user directly to wasp, untouched.
getImportDetailsForOperationUserJsFn ::
  Wasp.Operation.Operation ->
  -- | Relative posix path from js file where you want to do importing to generated ext code dir.
  -- | (importIdentifier, importStmt)
  -- - importIdentifier -> Identifier via which you can access js function after you import it with importStmt.
  -- - importStmt -> Import statement via which you should do the import.
  FilePath ->
  (String, String)
getImportDetailsForOperationUserJsFn operation relPosixPathToExtCodeDir = (importIdentifier, importStmt)
  where
    importStmt = "import " ++ importWhat ++ " from '" ++ importFrom ++ "'"
    importFrom = relPosixPathToExtCodeDir ++ SP.toFilePath (Wasp.JsImport._from jsImport)
    (importIdentifier, importWhat) =
      case (Wasp.JsImport._defaultImport jsImport, Wasp.JsImport._namedImports jsImport) of
        (Just defaultImport, []) -> (defaultImport, defaultImport)
        (Nothing, [namedImport]) -> (namedImport, "{ " ++ namedImport ++ " }")
        _ -> error "Expected either default import or single named import for operation (query/action) js function."
    jsImport = Wasp.Operation.getJsFn operation
