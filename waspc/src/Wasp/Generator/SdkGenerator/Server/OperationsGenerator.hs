{-# LANGUAGE TypeApplications #-}

module Wasp.Generator.SdkGenerator.Server.OperationsGenerator
  ( serverOperationsDirInSdkRootDir,
    genOperations,
  )
where

import Data.Aeson (object, (.=))
import qualified Data.Aeson as Aeson
import Data.List (nub)
import Data.Maybe (fromMaybe)
import StrongPath (Dir, Dir', File', Path', Rel, reldir, relfile, (</>))
import qualified StrongPath as SP
import Wasp.AppSpec (AppSpec)
import qualified Wasp.AppSpec as AS
import qualified Wasp.AppSpec.Action as AS.Action
import Wasp.AppSpec.Operation (getName)
import qualified Wasp.AppSpec.Operation as AS.Operation
import qualified Wasp.AppSpec.Query as AS.Query
import Wasp.AppSpec.Valid (isAuthEnabled)
import Wasp.Generator.Common (makeJsonWithEntityData)
import Wasp.Generator.FileDraft (FileDraft)
import Wasp.Generator.Monad (Generator)
import Wasp.Generator.SdkGenerator.Common (SdkTemplatesDir, getOperationTypeName, mkTmplFdWithData, serverTemplatesDirInSdkTemplatesDir)
import qualified Wasp.Generator.SdkGenerator.Common as C
import Wasp.Generator.SdkGenerator.JsImport (extOperationImportToImportJson)
import Wasp.Util (toUpperFirst)

data ServerOpsTemplatesDir

serverOpsDirInSdkTemplatesDir :: Path' (Rel SdkTemplatesDir) (Dir ServerOpsTemplatesDir)
serverOpsDirInSdkTemplatesDir = serverTemplatesDirInSdkTemplatesDir </> [reldir|operations|]

genOperations :: AppSpec -> Generator [FileDraft]
genOperations spec =
  sequence
    [ genQueryTypesFile spec,
      genActionTypesFile spec,
      genQueriesIndex spec,
      genActionsIndex spec,
      genWrappers spec,
      genIndexTs spec
    ]

genIndexTs :: AppSpec -> Generator FileDraft
genIndexTs spec = return $ mkTmplFdWithData relPath tmplData
  where
    relPath = serverOpsDirInSdkTemplatesDir </> [relfile|index.ts|]
    tmplData =
      object
        [ "actions" .= map (getActionData isAuthEnabledGlobally) (AS.getActions spec),
          "queries" .= map (getQueryData isAuthEnabledGlobally) (AS.getQueries spec)
        ]
    isAuthEnabledGlobally = isAuthEnabled spec

genWrappers :: AppSpec -> Generator FileDraft
genWrappers spec = return $ mkTmplFdWithData relPath tmplData
  where
    relPath = serverOpsDirInSdkTemplatesDir </> [relfile|wrappers.ts|]
    tmplData = object ["isAuthEnabled" .= isAuthEnabled spec]

genQueriesIndex :: AppSpec -> Generator FileDraft
genQueriesIndex spec = return $ mkTmplFdWithData relPath tmplData
  where
    relPath = serverOpsDirInSdkTemplatesDir </> [relfile|queries/index.ts|]
    tmplData =
      object
        [ "isAuthEnabled" .= isAuthEnabledGlobally,
          "operations" .= map (getQueryData isAuthEnabledGlobally) (AS.getQueries spec)
        ]
    isAuthEnabledGlobally = isAuthEnabled spec

genActionsIndex :: AppSpec -> Generator FileDraft
genActionsIndex spec = return $ mkTmplFdWithData relPath tmplData
  where
    relPath = serverOpsDirInSdkTemplatesDir </> [relfile|actions/index.ts|]
    tmplData =
      object
        [ "isAuthEnabled" .= isAuthEnabledGlobally,
          "operations" .= map (getActionData isAuthEnabledGlobally) (AS.getActions spec)
        ]
    isAuthEnabledGlobally = isAuthEnabled spec

genQueryTypesFile :: AppSpec -> Generator FileDraft
genQueryTypesFile spec = genOperationTypesFile relPath operations isAuthEnabledGlobally
  where
    relPath = serverOpsDirInSdkTemplatesDir </> [relfile|queries/types.ts|]
    operations = map (uncurry AS.Operation.QueryOp) $ AS.getQueries spec
    isAuthEnabledGlobally = isAuthEnabled spec

genActionTypesFile :: AppSpec -> Generator FileDraft
genActionTypesFile spec = genOperationTypesFile relPath operations isAuthEnabledGlobally
  where
    relPath = serverOpsDirInSdkTemplatesDir </> [relfile|actions/types.ts|]
    operations = map (uncurry AS.Operation.ActionOp) $ AS.getActions spec
    isAuthEnabledGlobally = isAuthEnabled spec

-- | Here we generate JS file that basically imports JS query function provided by user,
--   decorates it (mostly injects stuff into it) and exports. Idea is that the rest of the server,
--   and user also, should use this new JS function, and not the old one directly.
getQueryData :: Bool -> (String, AS.Query.Query) -> Aeson.Value
getQueryData isAuthEnabledGlobally (queryName, query) = getOperationTmplData isAuthEnabledGlobally operation
  where
    operation = AS.Operation.QueryOp queryName query

getActionData :: Bool -> (String, AS.Action.Action) -> Aeson.Value
getActionData isAuthEnabledGlobally (actionName, action) = getOperationTmplData isAuthEnabledGlobally operation
  where
    operation = AS.Operation.ActionOp actionName action

genOperationTypesFile ::
  Path' (Rel C.SdkTemplatesDir) File' ->
  [AS.Operation.Operation] ->
  Bool ->
  Generator FileDraft
genOperationTypesFile tmplFile operations isAuthEnabledGlobally =
  return $ C.mkTmplFdWithData tmplFile tmplData
  where
    tmplData =
      object
        [ "operations" .= map operationTypeData operations,
          "shouldImportAuthenticatedOperation" .= any usesAuth operations,
          "shouldImportNonAuthenticatedOperation" .= not (all usesAuth operations),
          "allEntities" .= nub (concatMap getEntities operations)
        ]
    operationTypeData operation =
      object
        [ "typeName" .= toUpperFirst (getName operation),
          "entities" .= getEntities operation,
          "usesAuth" .= usesAuth operation
        ]
    getEntities = map makeJsonWithEntityData . maybe [] (map AS.refName) . AS.Operation.getEntities
    usesAuth = fromMaybe isAuthEnabledGlobally . AS.Operation.getAuth

serverOperationsDirInSdkRootDir :: AS.Operation.Operation -> Path' (Rel C.SdkRootDir) Dir'
serverOperationsDirInSdkRootDir =
  SP.castRel . (serverOpsDirInSdkTemplatesDir </>) . \case
    (AS.Operation.QueryOp _ _) -> [reldir|queries|]
    (AS.Operation.ActionOp _ _) -> [reldir|actions|]

getOperationTmplData :: Bool -> AS.Operation.Operation -> Aeson.Value
getOperationTmplData isAuthEnabledGlobally operation =
  object
    [ "jsFn" .= extOperationImportToImportJson (AS.Operation.getFn operation),
      "operationName" .= getName operation,
      "operationTypeName" .= getOperationTypeName operation,
      "entities"
        .= maybe [] (map (makeJsonWithEntityData . AS.refName)) (AS.Operation.getEntities operation),
      "usesAuth" .= fromMaybe isAuthEnabledGlobally (AS.Operation.getAuth operation)
    ]
