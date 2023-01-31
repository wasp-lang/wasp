{-# LANGUAGE TypeApplications #-}

module Wasp.Generator.ServerGenerator.OperationsRoutesG
  ( genOperationsRoutes,
    operationRouteInOperationsRouter,
  )
where

import Data.Aeson (object, (.=))
import qualified Data.Aeson as Aeson
import Data.Maybe (fromJust, fromMaybe, isJust)
import StrongPath (Dir, File', Path, Path', Posix, Rel, reldir, reldirP, relfile, (</>))
import qualified StrongPath as SP
import Wasp.AppSpec (AppSpec)
import qualified Wasp.AppSpec as AS
import qualified Wasp.AppSpec.Action as AS.Action
import qualified Wasp.AppSpec.App as AS.App
import qualified Wasp.AppSpec.App.Auth as AS.Auth
import qualified Wasp.AppSpec.Operation as AS.Operation
import qualified Wasp.AppSpec.Query as AS.Query
import Wasp.AppSpec.Valid (getApp, isAuthEnabled)
import Wasp.Generator.FileDraft (FileDraft)
import Wasp.Generator.Monad (Generator, GeneratorError (GenericGeneratorError), logAndThrowGeneratorError)
import qualified Wasp.Generator.ServerGenerator.Common as C
import Wasp.Generator.ServerGenerator.OperationsG (operationFileInSrcDir)
import Wasp.JsImport (JsImportName (..), getJsImport, getJsImportStmtAndIdentifier)
import qualified Wasp.Util as U

genOperationsRoutes :: AppSpec -> Generator [FileDraft]
genOperationsRoutes spec =
  sequence . concat $
    [ map (genActionRoute spec) (AS.getActions spec),
      map (genQueryRoute spec) (AS.getQueries spec),
      [genOperationsRouter spec]
    ]

genActionRoute :: AppSpec -> (String, AS.Action.Action) -> Generator FileDraft
genActionRoute spec (actionName, action) = genOperationRoute spec op tmplFile
  where
    op = AS.Operation.ActionOp actionName action
    tmplFile = C.asTmplFile [relfile|src/routes/operations/_action.js|]

genQueryRoute :: AppSpec -> (String, AS.Query.Query) -> Generator FileDraft
genQueryRoute spec (queryName, query) = genOperationRoute spec op tmplFile
  where
    op = AS.Operation.QueryOp queryName query
    tmplFile = C.asTmplFile [relfile|src/routes/operations/_query.js|]

genOperationRoute :: AppSpec -> AS.Operation.Operation -> Path' (Rel C.ServerTemplatesDir) File' -> Generator FileDraft
genOperationRoute spec operation tmplFile = return $ C.mkTmplFdWithDstAndData tmplFile dstFile (Just tmplData)
  where
    dstFile = operationsRoutesDirInServerRootDir </> operationRouteFileInOperationsRoutesDir operation

    baseTmplData =
      object
        [ "operationName" .= (operationImportIdentifier :: String),
          "operationImportStmt" .= (operationImportStmt :: String)
        ]

    tmplData = case AS.App.auth (snd $ getApp spec) of
      Nothing -> baseTmplData
      Just auth ->
        U.jsonSet
          "userEntityLower"
          (Aeson.toJSON (U.toLowerFirst $ AS.refName $ AS.Auth.userEntity auth))
          baseTmplData

    operationImportPath =
      relPosixPathFromOperationsRoutesDirToSrcDir
        </> fromJust (SP.relFileToPosix $ operationFileInSrcDir operation)

    operationESModulesImportPath =
      fromJust $
        SP.parseRelFileP $
          C.toESModulesImportPath $
            SP.fromRelFileP operationImportPath

    operationName = AS.Operation.getName operation

    (operationImportIdentifier, operationImportStmt) = getJsImportStmtAndIdentifier $ getJsImport operationESModulesImportPath (JsImportModule operationName)

data OperationsRoutesDir

operationsRoutesDirInServerSrcDir :: Path' (Rel C.ServerSrcDir) (Dir OperationsRoutesDir)
operationsRoutesDirInServerSrcDir = [reldir|routes/operations/|]

operationsRoutesDirInServerRootDir :: Path' (Rel C.ServerRootDir) (Dir OperationsRoutesDir)
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
          (importIdentifier, importStmt) = getJsImportStmtAndIdentifier $ getJsImport importPath (JsImportModule operationName)
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
