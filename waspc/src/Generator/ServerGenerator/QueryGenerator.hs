module Generator.ServerGenerator.QueryGenerator
    ( genQueries
    , queryRouteInQueriesRouter
    ) where

import Data.Maybe (fromJust)
import Data.Aeson ((.=), object)
import qualified Path as P

import qualified Util as U
import StrongPath (Path, Rel, File, Dir, (</>))
import qualified StrongPath as SP
import Wasp (Wasp)
import qualified Wasp
import qualified Wasp.Query
import qualified Wasp.JsImport
import Generator.FileDraft (FileDraft)
import qualified Generator.ServerGenerator.Common as C

genQueries :: Wasp -> [FileDraft]
genQueries = genQueryRoutes

genQueryRoutes :: Wasp -> [FileDraft]
genQueryRoutes wasp = concat
    [ map (genQueryRoute wasp) (Wasp.getQueries wasp)
    , [genQueriesRouter wasp]
    ]

genQueryRoute :: Wasp -> Wasp.Query.Query -> FileDraft
genQueryRoute _ query = C.makeTemplateFD tmplFile dstFile (Just tmplData)
  where
    tmplFile = C.asTmplFile [P.relfile|src/routes/queries/_query.js|]

    dstFile = queryRoutesDirInServerRootDir </> queryRouteFileInQueryRoutesDir query

    tmplData = object
        [ "queryJsFnImportStatement" .= ("import " ++ importWhat ++ " from '" ++ fromPath ++ "'")
        , "queryJsFnIdentifier" .= importIdentifier
        ]

    jsQueryImport = Wasp.Query._jsFunction query

    (importIdentifier, importWhat) =
        case (Wasp.JsImport._defaultImport jsQueryImport, Wasp.JsImport._namedImports jsQueryImport) of
            (Just defaultImport, []) -> (defaultImport, defaultImport)
            (Nothing, [namedImport]) -> (namedImport, "{ " ++ namedImport ++ " }")
            _ -> error "Expected either default import or single named import for query js function."

    fromPath = relPathToExtSrcDir ++ SP.toFilePath (Wasp.JsImport._from jsQueryImport)

data QueryRoutesDir

queryRoutesDirInServerSrcDir :: Path (Rel C.ServerSrcDir) (Dir QueryRoutesDir)
queryRoutesDirInServerSrcDir = SP.fromPathRelDir [P.reldir|routes/queries/|]

queryRoutesDirInServerRootDir :: Path (Rel C.ServerRootDir) (Dir QueryRoutesDir)
queryRoutesDirInServerRootDir = C.serverSrcDirInServerRootDir </> queryRoutesDirInServerSrcDir

-- | TODO: fromJust here could fail if query name is weird, we should handle that.
queryRouteFileInQueryRoutesDir :: Wasp.Query.Query -> Path (Rel QueryRoutesDir) File
queryRouteFileInQueryRoutesDir query = fromJust $ SP.parseRelFile $ Wasp.Query._name query ++ ".js"

-- | TODO: Make this not hardcoded! Maybe even use StrongPath? But I can't because of ../../ .
relPathToExtSrcDir :: FilePath
relPathToExtSrcDir = "../../ext-src/"

genQueriesRouter :: Wasp -> FileDraft
genQueriesRouter wasp = C.makeTemplateFD tmplFile dstFile (Just tmplData)
  where
    tmplFile = C.asTmplFile [P.relfile|src/routes/queries/index.js|]
    dstFile = queryRoutesDirInServerRootDir </> SP.fromPathRelFile [P.relfile|index.js|]
    tmplData = object
        [ "queryRoutes" .= map makeQueryRoute (Wasp.getQueries wasp)
        ]
    makeQueryRoute query =
        let queryName = Wasp.Query._name query
        in object
           [ "importIdentifier" .= queryName
           , "importPath" .= ("./" ++ SP.toFilePath (queryRouteFileInQueryRoutesDir query))
           , "routePath" .= ("/" ++ queryRouteInQueriesRouter query)
           ]

queryRouteInQueriesRouter :: Wasp.Query.Query -> String
queryRouteInQueriesRouter = U.camelToKebabCase . Wasp.Query._name
