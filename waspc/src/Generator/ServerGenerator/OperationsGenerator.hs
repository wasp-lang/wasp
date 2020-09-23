module Generator.ServerGenerator.OperationsGenerator
    ( genOperations
    , operationRouteInOperationsRouter
    ) where

import Data.Maybe (fromJust)
import Data.Aeson ((.=), object)
import qualified Path as P

import qualified Util as U
import StrongPath (Path, Rel, File, Dir, (</>))
import qualified StrongPath as SP
import Wasp (Wasp)
import qualified Wasp
import qualified Wasp.Action
import qualified Wasp.Query
import qualified Wasp.Operation
import qualified Wasp.JsImport
import Generator.FileDraft (FileDraft)
import qualified Generator.ServerGenerator.Common as C

genOperations :: Wasp -> [FileDraft]
genOperations = genOperationsRoutes

genOperationsRoutes :: Wasp -> [FileDraft]
genOperationsRoutes wasp = concat
    [ map (genActionRoute wasp) (Wasp.getActions wasp)
    , map (genQueryRoute wasp) (Wasp.getQueries wasp)
    , [genOperationsRouter wasp]
    ]

genActionRoute :: Wasp -> Wasp.Action.Action -> FileDraft
genActionRoute wasp action = genOperationRoute wasp op tmplFile
    where op = Wasp.Operation.ActionOp action
          tmplFile = C.asTmplFile [P.relfile|src/routes/operations/_action.js|]

genQueryRoute :: Wasp -> Wasp.Query.Query -> FileDraft
genQueryRoute wasp query = genOperationRoute wasp op tmplFile
    where op = Wasp.Operation.QueryOp query
          tmplFile = C.asTmplFile [P.relfile|src/routes/operations/_query.js|]

genOperationRoute :: Wasp -> Wasp.Operation.Operation -> Path (Rel C.ServerTemplatesDir) File -> FileDraft
genOperationRoute _ operation tmplFile = C.makeTemplateFD tmplFile dstFile (Just tmplData)
  where
    dstFile = operationsRoutesDirInServerRootDir </> operationRouteFileInOperationsRoutesDir operation
    tmplData = object
        [ "jsFnImportStatement" .= importStmt
        , "jsFnIdentifier" .= importIdentifier
        ]
    (importIdentifier, importStmt) = getImportDetailsForOperationJsFn operation
                                                                      relPathFromOperationsRoutesDirToExtSrcDir

-- | Given Wasp operation, it returns details on how to import its js function and use it.
getImportDetailsForOperationJsFn
    :: Wasp.Operation.Operation
    -> FilePath -- ^ Relative path from js file where you want to do importing to generated ext code dir.
    -> ( String -- ^ importIdentifier -> Identifier via which you can access js function after you import it with importStmt.
       , String -- ^ importStmt -> Import statement via which you should do the import.
       )
getImportDetailsForOperationJsFn operation relPathToExtCodeDir = (importIdentifier, importStmt)
  where
    importStmt = "import " ++ importWhat ++ " from '" ++ importFrom ++ "'"
    importFrom = relPathToExtCodeDir ++ SP.toFilePath (Wasp.JsImport._from jsImport)
    (importIdentifier, importWhat) =
        case (Wasp.JsImport._defaultImport jsImport, Wasp.JsImport._namedImports jsImport) of
            (Just defaultImport, []) -> (defaultImport, defaultImport)
            (Nothing, [namedImport]) -> (namedImport, "{ " ++ namedImport ++ " }")
            _ -> error "Expected either default import or single named import for operation (query/action) js function."
    jsImport = Wasp.Operation.getJsFn operation


data OperationsRoutesDir

operationsRoutesDirInServerSrcDir :: Path (Rel C.ServerSrcDir) (Dir OperationsRoutesDir)
operationsRoutesDirInServerSrcDir = SP.fromPathRelDir [P.reldir|routes/operations/|]

operationsRoutesDirInServerRootDir :: Path (Rel C.ServerRootDir) (Dir OperationsRoutesDir)
operationsRoutesDirInServerRootDir = C.serverSrcDirInServerRootDir </> operationsRoutesDirInServerSrcDir

operationRouteFileInOperationsRoutesDir :: Wasp.Operation.Operation -> Path (Rel OperationsRoutesDir) File
operationRouteFileInOperationsRoutesDir operation = fromJust $ SP.parseRelFile $ Wasp.Operation.getName operation ++ ".js"

-- | TODO: Make this not hardcoded! Maybe even use StrongPath? But I can't because of ../../ .
relPathFromOperationsRoutesDirToExtSrcDir :: FilePath
relPathFromOperationsRoutesDirToExtSrcDir = "../../ext-src/"


genOperationsRouter :: Wasp -> FileDraft
genOperationsRouter wasp = C.makeTemplateFD tmplFile dstFile (Just tmplData)
  where
    tmplFile = C.asTmplFile [P.relfile|src/routes/operations/index.js|]
    dstFile = operationsRoutesDirInServerRootDir </> SP.fromPathRelFile [P.relfile|index.js|]
    operations = map Wasp.Operation.ActionOp (Wasp.getActions wasp)
                 ++ map Wasp.Operation.QueryOp (Wasp.getQueries wasp)
    tmplData = object
        [ "operationRoutes" .= map makeOperationRoute operations
        ]
    makeOperationRoute operation =
        let operationName = Wasp.Operation.getName operation
        in object
           [ "importIdentifier" .= operationName
           , "importPath" .= ("./" ++ SP.toFilePath (operationRouteFileInOperationsRoutesDir operation))
           , "routePath" .= ("/" ++ operationRouteInOperationsRouter operation)
           ]

operationRouteInOperationsRouter :: Wasp.Operation.Operation -> String
operationRouteInOperationsRouter = U.camelToKebabCase . Wasp.Operation.getName
