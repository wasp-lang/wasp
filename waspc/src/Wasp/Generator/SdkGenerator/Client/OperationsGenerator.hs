module Wasp.Generator.SdkGenerator.Client.OperationsGenerator (genOperations) where

import Data.Aeson (KeyValue ((.=)), object)
import qualified Data.Aeson as Aeson
import Data.Aeson.Types (Pair)
import Data.Maybe (fromJust)
import StrongPath (Dir, File', Path', Rel, reldir, relfile, (</>))
import qualified StrongPath as SP
import Wasp.AppSpec (AppSpec (..))
import qualified Wasp.AppSpec as AS
import qualified Wasp.AppSpec.Action as AS.Action
import qualified Wasp.AppSpec.Operation as AS.Operation
import qualified Wasp.AppSpec.Query as AS.Query
import Wasp.Generator.Common (makeJsArrayFromHaskellList)
import Wasp.Generator.FileDraft (FileDraft)
import Wasp.Generator.Monad (Generator)
import Wasp.Generator.SdkGenerator.Client.Common
import Wasp.Generator.SdkGenerator.Common
import Wasp.Generator.SdkGenerator.Server.OperationsGenerator (serverOperationsDirInSdkRootDir)
import qualified Wasp.Generator.ServerGenerator as ServerGenerator
import qualified Wasp.Generator.ServerGenerator.OperationsRoutesG as ServerOperationsRoutesG
import Wasp.JsImport (JsImportName (JsImportField), JsImportPath (ModuleImportPath), getJsImportStmtAndIdentifier, makeJsImport)
import Wasp.Util ((<++>))

data ClientOpsTemplatesDir

genOperations :: AppSpec -> Generator [FileDraft]
genOperations spec =
  sequence
    [ -- Not migrated to TS yet
      genClientOpsFileCopy SdkUserCoreProject [relfile|internal/resources.js|],
      genClientOpsFileCopy SdkUserCoreProject [relfile|internal/index.ts|],
      -- Not migrated to TS yet
      genClientOpsFileCopy SdkUserCoreProject [relfile|internal/updateHandlersMap.js|],
      genClientOpsFileCopy SdkUserCoreProject [relfile|rpc.ts|],
      genClientOpsFileCopy SdkUserCoreProject [relfile|hooks.ts|],
      genClientOpsFileCopy SdkUserCoreProject [relfile|index.ts|],
      genClientOpsFileCopy SdkUserCoreProject [relfile|queryClient.ts|]
    ]
    <++> genQueries spec
    <++> genActions spec

genQueries :: AppSpec -> Generator [FileDraft]
genQueries spec =
  sequence
    [ genClientOpsFileCopy SdkUserCoreProject [relfile|queries/core.ts|],
      genQueriesIndex spec
    ]

genActions :: AppSpec -> Generator [FileDraft]
genActions spec =
  sequence
    [ genClientOpsFileCopy SdkUserCoreProject [relfile|actions/core.ts|],
      genActionsIndex spec
    ]

genQueriesIndex :: AppSpec -> Generator FileDraft
genQueriesIndex spec =
  return $ makeSdkProjectTmplFdWithData SdkUserCoreProject tmplFile tmplData
  where
    tmplFile = clientOpsDirInSdkTemplatesProjectDir </> [relfile|queries/index.ts|]
    tmplData =
      object
        [ "queries" .= map getQueryData (AS.getQueries spec)
        ]

genActionsIndex :: AppSpec -> Generator FileDraft
genActionsIndex spec =
  return $ makeSdkProjectTmplFdWithData SdkUserCoreProject tmplF tmplData
  where
    tmplF = clientOpsDirInSdkTemplatesProjectDir </> [relfile|actions/index.ts|]
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
            SP.relDirToPosix $
              serverOperationsDirInSdkRootDir operation

clientOpsDirInSdkTemplatesProjectDir :: Path' (Rel SdkTemplatesProjectDir) (Dir ClientOpsTemplatesDir)
clientOpsDirInSdkTemplatesProjectDir = clientTemplatesDirInSdkTemplatesDir </> [reldir|operations|]

genClientOpsFileCopy :: SdkProject -> Path' (Rel ClientOpsTemplatesDir) File' -> Generator FileDraft
genClientOpsFileCopy sdkProject =
  return . makeSdkProjectTmplFd sdkProject . (clientOpsDirInSdkTemplatesProjectDir </>)
