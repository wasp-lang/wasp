module Wasp.Generator.SdkGenerator.Client.OperationsGenerator (genOperations) where

import Data.Aeson (KeyValue ((.=)), object)
import qualified Data.Aeson as Aeson
import Data.Aeson.Types (Pair)
import Data.Maybe (fromJust)
import StrongPath (Dir, File', Path', Rel, relDirToPosix, reldir, relfile, (</>))
import Wasp.AppSpec (AppSpec (..))
import qualified Wasp.AppSpec as AS
import qualified Wasp.AppSpec.Action as AS.Action
import qualified Wasp.AppSpec.Operation as AS.Operation
import qualified Wasp.AppSpec.Query as AS.Query
import Wasp.Generator.Common (makeJsArrayFromHaskellList)
import Wasp.Generator.FileDraft (FileDraft)
import Wasp.Generator.Monad (Generator)
import Wasp.Generator.SdkGenerator.Common
  ( getOperationTypeName,
    makeSdkImportPath,
    relDirToRelFileP,
  )
import Wasp.Generator.SdkGenerator.Server.OperationsGenerator (serverOperationsDirInSdkRootDir)
import Wasp.Generator.SdkGenerator.UserCore.Common
  ( SdkTemplatesUserCoreDir,
    mkTmplFd,
    mkTmplFdWithData,
  )
import qualified Wasp.Generator.ServerGenerator as ServerGenerator
import qualified Wasp.Generator.ServerGenerator.OperationsRoutesG as ServerOperationsRoutesG
import Wasp.JsImport (JsImportName (JsImportField), JsImportPath (ModuleImportPath), getJsImportStmtAndIdentifier, makeJsImport)
import Wasp.Util ((<++>))

data ClientOpsTemplatesDir

genOperations :: AppSpec -> Generator [FileDraft]
genOperations spec =
  sequence
    [ -- Not migrated to TS yet
      genFileCopyInClientOps [relfile|internal/resources.js|],
      genFileCopyInClientOps [relfile|internal/index.ts|],
      -- Not migrated to TS yet
      genFileCopyInClientOps [relfile|internal/updateHandlersMap.js|],
      genFileCopyInClientOps [relfile|rpc.ts|],
      genFileCopyInClientOps [relfile|hooks.ts|],
      genFileCopyInClientOps [relfile|index.ts|],
      genFileCopyInClientOps [relfile|queryClient.ts|]
    ]
    <++> genQueries spec
    <++> genActions spec

genQueries :: AppSpec -> Generator [FileDraft]
genQueries spec =
  sequence
    [ genFileCopyInClientOps [relfile|queries/core.ts|],
      genQueriesIndex spec
    ]

genActions :: AppSpec -> Generator [FileDraft]
genActions spec =
  sequence
    [ genFileCopyInClientOps [relfile|actions/core.ts|],
      genActionsIndex spec
    ]

genQueriesIndex :: AppSpec -> Generator FileDraft
genQueriesIndex spec =
  return $ mkTmplFdWithData (clientOpsDirInSdkTemplatesUserCoreDir </> [relfile|queries/index.ts|]) tmplData
  where
    tmplData =
      object
        [ "queries" .= map getQueryData (AS.getQueries spec)
        ]

genActionsIndex :: AppSpec -> Generator FileDraft
genActionsIndex spec =
  return $ mkTmplFdWithData (clientOpsDirInSdkTemplatesUserCoreDir </> [relfile|actions/index.ts|]) tmplData
  where
    tmplData =
      object
        [ "actions" .= map getActionData (AS.getActions spec)
        ]

getQueryData :: (String, AS.Query.Query) -> Aeson.Value
getQueryData (queryName, query) =
  object $
    [ "queryRoute"
        .= ( ServerGenerator.operationsRouteInRootRouter
               ++ "/"
               ++ ServerOperationsRoutesG.operationRouteInOperationsRouter operation
           ),
      "entitiesArray" .= makeJsArrayOfEntityNames operation
    ]
      ++ getOperationTypeData operation
  where
    operation = AS.Operation.QueryOp queryName query

getActionData :: (String, AS.Action.Action) -> Aeson.Value
getActionData (actionName, action) =
  object $
    [ "actionRoute"
        .= ( ServerGenerator.operationsRouteInRootRouter
               ++ "/"
               ++ ServerOperationsRoutesG.operationRouteInOperationsRouter operation
           ),
      "entitiesArray" .= makeJsArrayOfEntityNames operation
    ]
      ++ getOperationTypeData operation
  where
    operation = AS.Operation.ActionOp actionName action

-- | Generates string that is JS array containing names (as strings) of entities being used by given operation.
--   E.g. "['Task', 'Project']"
makeJsArrayOfEntityNames :: AS.Operation.Operation -> String
makeJsArrayOfEntityNames operation = makeJsArrayFromHaskellList entityNames
  where
    entityNames = maybe [] (map $ \x -> AS.refName x) (AS.Operation.getEntities operation)

getOperationTypeData :: AS.Operation.Operation -> [Pair]
getOperationTypeData operation = tmplData
  where
    tmplData =
      [ "operationTypeImportStmt" .= operationTypeImportStmt,
        "operationTypeName" .= operationTypeImportIdentifier,
        "operationName" .= operationName
      ]

    operationName = AS.Operation.getName operation

    (operationTypeImportStmt, operationTypeImportIdentifier) =
      getJsImportStmtAndIdentifier $
        makeJsImport (ModuleImportPath serverOpsImportPath) (JsImportField $ getOperationTypeName operation)
    serverOpsImportPath =
      makeSdkImportPath $
        relDirToRelFileP $
          fromJust $
            relDirToPosix $
              serverOperationsDirInSdkRootDir operation

clientOpsDirInSdkTemplatesUserCoreDir :: Path' (Rel SdkTemplatesUserCoreDir) (Dir ClientOpsTemplatesDir)
clientOpsDirInSdkTemplatesUserCoreDir = [reldir|client/operations|]

genFileCopyInClientOps :: Path' (Rel ClientOpsTemplatesDir) File' -> Generator FileDraft
genFileCopyInClientOps =
  return . mkTmplFd . (clientOpsDirInSdkTemplatesUserCoreDir </>)
