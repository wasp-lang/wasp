module Generator.ServerGenerator.OperationsRoutesG
  ( genOperationsRoutes,
    operationRouteInOperationsRouter,
  )
where

import Data.Aeson (object, (.=))
import qualified Data.Aeson as Aeson
import Data.Maybe (fromJust, fromMaybe, isJust)
import Generator.FileDraft (FileDraft)
import qualified Generator.ServerGenerator.Common as C
import Generator.ServerGenerator.OperationsG (operationFileInSrcDir)
import StrongPath (Dir, File', Path, Path', Posix, Rel, reldir, reldirP, relfile, (</>))
import qualified StrongPath as SP
import qualified Util as U
import Wasp (Wasp, getAuth)
import qualified Wasp
import qualified Wasp.Action
import qualified Wasp.Auth
import qualified Wasp.Operation
import qualified Wasp.Query

genOperationsRoutes :: Wasp -> [FileDraft]
genOperationsRoutes wasp =
  concat
    [ map (genActionRoute wasp) (Wasp.getActions wasp),
      map (genQueryRoute wasp) (Wasp.getQueries wasp),
      [genOperationsRouter wasp]
    ]

genActionRoute :: Wasp -> Wasp.Action.Action -> FileDraft
genActionRoute wasp action = genOperationRoute wasp op tmplFile
  where
    op = Wasp.Operation.ActionOp action
    tmplFile = C.asTmplFile [relfile|src/routes/operations/_action.js|]

genQueryRoute :: Wasp -> Wasp.Query.Query -> FileDraft
genQueryRoute wasp query = genOperationRoute wasp op tmplFile
  where
    op = Wasp.Operation.QueryOp query
    tmplFile = C.asTmplFile [relfile|src/routes/operations/_query.js|]

genOperationRoute :: Wasp -> Wasp.Operation.Operation -> Path' (Rel C.ServerTemplatesDir) File' -> FileDraft
genOperationRoute wasp operation tmplFile = C.makeTemplateFD tmplFile dstFile (Just tmplData)
  where
    dstFile = operationsRoutesDirInServerRootDir </> operationRouteFileInOperationsRoutesDir operation

    baseTmplData =
      object
        [ "operationImportPath" .= operationImportPath,
          "operationName" .= Wasp.Operation.getName operation
        ]

    tmplData = case Wasp.getAuth wasp of
      Nothing -> baseTmplData
      Just auth ->
        U.jsonSet
          "userEntityLower"
          (Aeson.toJSON (U.toLowerFirst $ Wasp.Auth._userEntity auth))
          baseTmplData

    operationImportPath =
      SP.fromRelFileP $
        relPosixPathFromOperationsRoutesDirToSrcDir
          </> fromJust (SP.relFileToPosix $ operationFileInSrcDir operation)

data OperationsRoutesDir

operationsRoutesDirInServerSrcDir :: Path' (Rel C.ServerSrcDir) (Dir OperationsRoutesDir)
operationsRoutesDirInServerSrcDir = [reldir|routes/operations/|]

operationsRoutesDirInServerRootDir :: Path' (Rel C.ServerRootDir) (Dir OperationsRoutesDir)
operationsRoutesDirInServerRootDir = C.serverSrcDirInServerRootDir </> operationsRoutesDirInServerSrcDir

operationRouteFileInOperationsRoutesDir :: Wasp.Operation.Operation -> Path' (Rel OperationsRoutesDir) File'
operationRouteFileInOperationsRoutesDir operation = fromJust $ SP.parseRelFile $ Wasp.Operation.getName operation ++ ".js"

relPosixPathFromOperationsRoutesDirToSrcDir :: Path Posix (Rel OperationsRoutesDir) (Dir C.ServerSrcDir)
relPosixPathFromOperationsRoutesDirToSrcDir = [reldirP|../..|]

genOperationsRouter :: Wasp -> FileDraft
genOperationsRouter wasp
  -- TODO: Right now we are throwing error here, but we should instead perform this check in parsing/analyzer phase, as a semantic check, since we have all the info we need then already.
  | any isAuthSpecifiedForOperation operations && not isAuthEnabledGlobally = error "`auth` cannot be specified for specific operations if it is not enabled for the whole app!"
  | otherwise = C.makeTemplateFD tmplFile dstFile (Just tmplData)
  where
    tmplFile = C.asTmplFile [relfile|src/routes/operations/index.js|]
    dstFile = operationsRoutesDirInServerRootDir </> [relfile|index.js|]
    operations =
      map Wasp.Operation.ActionOp (Wasp.getActions wasp)
        ++ map Wasp.Operation.QueryOp (Wasp.getQueries wasp)
    tmplData =
      object
        [ "operationRoutes" .= map makeOperationRoute operations,
          "isAuthEnabled" .= isAuthEnabledGlobally
        ]
    makeOperationRoute operation =
      let operationName = Wasp.Operation.getName operation
       in object
            [ "importIdentifier" .= operationName,
              "importPath" .= ("./" ++ SP.fromRelFileP (fromJust $ SP.relFileToPosix $ operationRouteFileInOperationsRoutesDir operation)),
              "routePath" .= ("/" ++ operationRouteInOperationsRouter operation),
              "isUsingAuth" .= isAuthEnabledForOperation operation
            ]

    isAuthEnabledGlobally = isJust $ getAuth wasp
    isAuthEnabledForOperation operation = fromMaybe isAuthEnabledGlobally (Wasp.Operation.getAuth operation)
    isAuthSpecifiedForOperation operation = isJust $ Wasp.Operation.getAuth operation

operationRouteInOperationsRouter :: Wasp.Operation.Operation -> String
operationRouteInOperationsRouter = U.camelToKebabCase . Wasp.Operation.getName
