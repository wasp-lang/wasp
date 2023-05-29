{-# LANGUAGE TypeApplications #-}

module Wasp.Generator.ServerGenerator.OperationsRoutesG
  ( genOperationsRoutes,
    operationRouteInOperationsRouter,
  )
where

import Data.Aeson (object, (.=))
import Data.Maybe (fromJust, fromMaybe, isJust)
import StrongPath (Dir, File', Path, Path', Posix, Rel, reldir, reldirP, relfile, (</>))
import qualified StrongPath as SP
import Wasp.AppSpec (AppSpec)
import qualified Wasp.AppSpec as AS
import qualified Wasp.AppSpec.Action as AS.Action
import qualified Wasp.AppSpec.Operation as AS.Operation
import qualified Wasp.AppSpec.Query as AS.Query
import Wasp.AppSpec.Valid (isAuthEnabled)
import Wasp.Generator.Common (ServerRootDir)
import Wasp.Generator.FileDraft (FileDraft)
import Wasp.Generator.Monad (Generator, GeneratorError (GenericGeneratorError), logAndThrowGeneratorError)
import qualified Wasp.Generator.ServerGenerator.Common as C
import Wasp.Generator.ServerGenerator.OperationsG (operationFileInSrcDir)
import Wasp.JsImport (JsImportName (..), getJsImportStmtAndIdentifier, makeJsImport)
import qualified Wasp.Util as U

genOperationsRoutes :: AppSpec -> Generator [FileDraft]
genOperationsRoutes spec =
  sequence . concat $
    [ map genActionRoute (AS.getActions spec),
      map genQueryRoute (AS.getQueries spec),
      [genOperationsRouter spec]
    ]

genActionRoute :: (String, AS.Action.Action) -> Generator FileDraft
genActionRoute (actionName, action) = genOperationRoute op tmplFile
  where
    op = AS.Operation.ActionOp actionName action
    tmplFile = C.asTmplFile [relfile|src/routes/operations/_action.js|]

genQueryRoute :: (String, AS.Query.Query) -> Generator FileDraft
genQueryRoute (queryName, query) = genOperationRoute op tmplFile
  where
    op = AS.Operation.QueryOp queryName query
    tmplFile = C.asTmplFile [relfile|src/routes/operations/_query.js|]

genOperationRoute :: AS.Operation.Operation -> Path' (Rel C.ServerTemplatesDir) File' -> Generator FileDraft
genOperationRoute operation tmplFile = return $ C.mkTmplFdWithDstAndData tmplFile dstFile (Just tmplData)
  where
    dstFile = operationsRoutesDirInServerRootDir </> operationRouteFileInOperationsRoutesDir operation

    tmplData =
      object
        [ "operationName" .= (operationImportIdentifier :: String),
          "operationImportStmt" .= (operationImportStmt :: String)
        ]

    pathToOperationFile =
      relPosixPathFromOperationsRoutesDirToSrcDir
        </> fromJust (SP.relFileToPosix $ operationFileInSrcDir operation)

    operationImportPath =
      fromJust $
        SP.parseRelFileP $
          C.toESModulesImportPath $
            SP.fromRelFileP pathToOperationFile

    operationName = AS.Operation.getName operation

    (operationImportStmt, operationImportIdentifier) =
      getJsImportStmtAndIdentifier $
        makeJsImport operationImportPath (JsImportModule operationName)

data OperationsRoutesDir

operationsRoutesDirInServerSrcDir :: Path' (Rel C.ServerSrcDir) (Dir OperationsRoutesDir)
operationsRoutesDirInServerSrcDir = [reldir|routes/operations/|]

operationsRoutesDirInServerRootDir :: Path' (Rel ServerRootDir) (Dir OperationsRoutesDir)
operationsRoutesDirInServerRootDir = C.serverSrcDirInServerRootDir </> operationsRoutesDirInServerSrcDir

operationRouteFileInOperationsRoutesDir :: AS.Operation.Operation -> Path' (Rel OperationsRoutesDir) File'
operationRouteFileInOperationsRoutesDir operation = fromJust $ SP.parseRelFile $ AS.Operation.getName operation ++ ".js"

relPosixPathFromOperationsRoutesDirToSrcDir :: Path Posix (Rel OperationsRoutesDir) (Dir C.ServerSrcDir)
relPosixPathFromOperationsRoutesDirToSrcDir = [reldirP|../..|]

genOperationsRouter :: AppSpec -> Generator FileDraft
genOperationsRouter spec
  -- TODO: Right now we are throwing error here, but we should instead perform this check in parsing/analyzer phase, as a semantic check, since we have all the info we need then already.
  | any isAuthSpecifiedForOperation operations && not isAuthEnabledGlobally = logAndThrowGeneratorError $ GenericGeneratorError "`auth` cannot be specified for specific operations if it is not enabled for the whole app!"
  | otherwise = return $ C.mkTmplFdWithDstAndData tmplFile dstFile (Just tmplData)
  where
    tmplFile = C.asTmplFile [relfile|src/routes/operations/index.js|]
    dstFile = operationsRoutesDirInServerRootDir </> [relfile|index.js|]
    operations =
      map (uncurry AS.Operation.ActionOp) (AS.getActions spec)
        ++ map (uncurry AS.Operation.QueryOp) (AS.getQueries spec)
    tmplData =
      object
        [ "operationRoutes" .= map makeOperationRoute operations,
          "isAuthEnabled" .= isAuthEnabledGlobally
        ]
    makeOperationRoute operation =
      let operationName = AS.Operation.getName operation
          importPath = fromJust $ SP.relFileToPosix $ SP.castRel $ operationRouteFileInOperationsRoutesDir operation
          (importStmt, importIdentifier) = getJsImportStmtAndIdentifier $ makeJsImport importPath (JsImportModule operationName)
       in object
            [ "importIdentifier" .= importIdentifier,
              "importStatement" .= importStmt,
              "routePath" .= ("/" ++ operationRouteInOperationsRouter operation),
              "isUsingAuth" .= isAuthEnabledForOperation operation
            ]

    isAuthEnabledGlobally = isAuthEnabled spec
    isAuthEnabledForOperation operation = fromMaybe isAuthEnabledGlobally (AS.Operation.getAuth operation)
    isAuthSpecifiedForOperation operation = isJust $ AS.Operation.getAuth operation

operationRouteInOperationsRouter :: AS.Operation.Operation -> String
operationRouteInOperationsRouter = U.camelToKebabCase . AS.Operation.getName
