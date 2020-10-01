module Generator.ServerGenerator.OperationsG
    ( genOperations
    , queryFileInSrcDir
    , actionFileInSrcDir
    , operationFileInSrcDir
    ) where

import           Data.Aeson                       (object, (.=))
import qualified Data.Aeson                       as Aeson
import           Data.Char                        (toLower)
import           Data.Maybe                       (fromJust, fromMaybe)
import qualified Path                             as P

import           Generator.FileDraft              (FileDraft)
import qualified Generator.ServerGenerator.Common as C
import           StrongPath                       (File, Path, Rel, (</>))
import qualified StrongPath                       as SP
import           Wasp                             (Wasp)
import qualified Wasp
import qualified Wasp.Action
import qualified Wasp.JsImport
import qualified Wasp.Operation
import qualified Wasp.Query


genOperations :: Wasp -> [FileDraft]
genOperations wasp = concat
    [ genQueries wasp
    , genActions wasp
    ]

genQueries :: Wasp -> [FileDraft]
genQueries wasp = concat
    [ map (genQuery wasp) (Wasp.getQueries wasp)
    ]

genActions :: Wasp -> [FileDraft]
genActions wasp = concat
    [ map (genAction wasp) (Wasp.getActions wasp)
    ]

genQuery :: Wasp -> Wasp.Query.Query -> FileDraft
genQuery _ query = C.makeTemplateFD tmplFile dstFile (Just tmplData)
  where
    operation = Wasp.Operation.QueryOp query
    tmplFile = C.asTmplFile [P.relfile|src/queries/_query.js|]
    dstFile = C.serverSrcDirInServerRootDir </> queryFileInSrcDir query
    tmplData = object
        [ "jsFnImportStatement" .= importStmt
        , "jsFnIdentifier" .= importIdentifier
        , "entities" .= map buildEntityData (fromMaybe [] $ Wasp.Operation.getEntities operation)
        ]
    (importIdentifier, importStmt) = getImportDetailsForOperationUserJsFn operation relPosixPathFromQueriesDirToExtSrcDir
    buildEntityData :: String -> Aeson.Value
    buildEntityData entityName = object [ "name" .= entityName
                                        , "prismaIdentifier" .= (toLower (head entityName) : tail entityName)
                                        ]

queryFileInSrcDir :: Wasp.Query.Query -> Path (Rel C.ServerSrcDir) File
queryFileInSrcDir query = SP.fromPathRelFile $
    [P.reldir|queries|]
    -- | TODO: fromJust here could fail if there is some problem with the name, we should handle this.
    P.</> fromJust (P.parseRelFile $ Wasp.Query._name query ++ ".js")

-- TODO: This is very much duplicate of genQuery above, consider removing this duplication.
genAction :: Wasp -> Wasp.Action.Action -> FileDraft
genAction _ action = C.makeTemplateFD tmplFile dstFile (Just tmplData)
  where
    operation = Wasp.Operation.ActionOp action
    tmplFile = C.asTmplFile [P.relfile|src/actions/_action.js|]
    dstFile = C.serverSrcDirInServerRootDir </> actionFileInSrcDir action
    tmplData = object
        [ "jsFnImportStatement" .= importStmt
        , "jsFnIdentifier" .= importIdentifier
        , "entities" .= map buildEntityData (fromMaybe [] $ Wasp.Operation.getEntities operation)
        ]
    (importIdentifier, importStmt) = getImportDetailsForOperationUserJsFn operation relPosixPathFromActionsDirToExtSrcDir
    buildEntityData :: String -> Aeson.Value
    buildEntityData entityName = object [ "name" .= entityName
                                        , "prismaIdentifier" .= (toLower (head entityName) : tail entityName)
                                        ]

-- TODO: This is very much duplicate of queryFileInSrcDir above, consider removing this duplication.
actionFileInSrcDir :: Wasp.Action.Action -> Path (Rel C.ServerSrcDir) File
actionFileInSrcDir action = SP.fromPathRelFile $
    [P.reldir|actions|]
    -- | TODO: fromJust here could fail if there is some problem with the name, we should handle this.
    P.</> fromJust (P.parseRelFile $ Wasp.Action._name action ++ ".js")

operationFileInSrcDir :: Wasp.Operation.Operation -> Path (Rel C.ServerSrcDir) File
operationFileInSrcDir (Wasp.Operation.QueryOp query) = queryFileInSrcDir query
operationFileInSrcDir (Wasp.Operation.ActionOp action) = actionFileInSrcDir action

-- | TODO: Make this not hardcoded! Maybe even use StrongPath? But I can't because of "../" .
relPosixPathFromQueriesDirToExtSrcDir :: FilePath -- Posix
relPosixPathFromQueriesDirToExtSrcDir = "../ext-src/"
relPosixPathFromActionsDirToExtSrcDir :: FilePath -- Posix
relPosixPathFromActionsDirToExtSrcDir = "../ext-src/"

-- | Given Wasp operation, it returns details on how to import its user js function and use it,
--   "user js function" meaning the one provided by user directly to wasp, untouched.
getImportDetailsForOperationUserJsFn
    :: Wasp.Operation.Operation
    -> FilePath -- ^ Relative posix path from js file where you want to do importing to generated ext code dir.
    -> ( String -- ^ importIdentifier -> Identifier via which you can access js function after you import it with importStmt.
       , String -- ^ importStmt -> Import statement via which you should do the import.
       )
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
