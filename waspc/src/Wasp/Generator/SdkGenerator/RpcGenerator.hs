module Wasp.Generator.SdkGenerator.RpcGenerator (genRpc) where

import Data.Aeson (KeyValue ((.=)), object)
import qualified Data.Aeson as Aeson
import Data.Aeson.Types (Pair)
import Data.Maybe (fromJust)
import StrongPath (relfile)
import qualified StrongPath as SP
import Wasp.AppSpec (AppSpec (..))
import qualified Wasp.AppSpec as AS
import qualified Wasp.AppSpec.Action as AS.Action
import qualified Wasp.AppSpec.Operation as AS.Operation
import qualified Wasp.AppSpec.Query as AS.Query
import Wasp.Generator.Common (makeJsArrayFromHaskellList)
import Wasp.Generator.FileDraft (FileDraft)
import Wasp.Generator.Monad (Generator)
import Wasp.Generator.SdkGenerator.Common (makeSdkImportPath, relDirToRelFileP)
import qualified Wasp.Generator.SdkGenerator.Common as C
import Wasp.Generator.SdkGenerator.ServerOpsGenerator (serverOperationsDirInSdkRootDir)
import qualified Wasp.Generator.ServerGenerator as ServerGenerator
import qualified Wasp.Generator.ServerGenerator.OperationsRoutesG as ServerOperationsRoutesG
import Wasp.JsImport (JsImportName (JsImportField), JsImportPath (ModuleImportPath), getJsImportStmtAndIdentifier, makeJsImport)
import Wasp.Util (toUpperFirst, (<++>))

genRpc :: AppSpec -> Generator [FileDraft]
genRpc spec =
  sequence
    [ genFileCopy [relfile|rpc/index.ts|],
      genFileCopy [relfile|rpc/queryClient.ts|]
    ]
    <++> genQueries spec
    <++> genActions spec
  where
    genFileCopy = return . C.mkTmplFd

genQueries :: AppSpec -> Generator [FileDraft]
genQueries spec =
  (:) <$> genQueriesIndex spec
    <*> return
      [ C.mkTmplFd [relfile|rpc/queries/core.js|],
        C.mkTmplFd [relfile|rpc/queries/core.d.ts|]
      ]

genActions :: AppSpec -> Generator [FileDraft]
genActions spec =
  (:) <$> genActionsIndex spec
    <*> return
      [ C.mkTmplFd [relfile|rpc/actions/core.js|],
        C.mkTmplFd [relfile|rpc/actions/core.d.ts|]
      ]

genQueriesIndex :: AppSpec -> Generator FileDraft
genQueriesIndex spec = return $ C.mkTmplFdWithData relPath tmplData
  where
    relPath = [relfile|rpc/queries/index.ts|]
    tmplData =
      object
        [ "queries" .= map getQueryData (AS.getQueries spec)
        ]

genActionsIndex :: AppSpec -> Generator FileDraft
genActionsIndex spec = return $ C.mkTmplFdWithData relPath tmplData
  where
    relPath = [relfile|rpc/actions/index.ts|]
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
        makeJsImport (ModuleImportPath serverOpsImportPath) (JsImportField $ toUpperFirst operationName)
    serverOpsImportPath =
      makeSdkImportPath $
        relDirToRelFileP $
          fromJust $
            SP.relDirToPosix $ serverOperationsDirInSdkRootDir operation
