{-# LANGUAGE TypeApplications #-}

module Wasp.Generator.SdkGenerator.ServerOpsGenerator where

import Data.Aeson (object, (.=))
import qualified Data.Aeson as Aeson
import Data.List (nub)
import Data.Maybe (fromJust, fromMaybe)
import StrongPath (Dir', File', Path', Rel, reldir, relfile, (</>))
import qualified StrongPath as SP
import Wasp.AppSpec (AppSpec)
import qualified Wasp.AppSpec as AS
import qualified Wasp.AppSpec.Action as AS.Action
import qualified Wasp.AppSpec.ExtImport as EI
import Wasp.AppSpec.Operation (getName)
import qualified Wasp.AppSpec.Operation as AS.Operation
import qualified Wasp.AppSpec.Query as AS.Query
import Wasp.AppSpec.Valid (isAuthEnabled)
import Wasp.Generator.Common (makeJsonWithEntityData)
import Wasp.Generator.FileDraft (FileDraft)
import qualified Wasp.Generator.JsImport as GJI
import Wasp.Generator.Monad (Generator)
import Wasp.Generator.SdkGenerator.Common (mkTmplFdWithData)
import qualified Wasp.Generator.SdkGenerator.Common as C
import Wasp.JsImport (JsImport (..), JsImportPath (..))
import qualified Wasp.JsImport as JI
import Wasp.Util (toUpperFirst)

genOperations :: AppSpec -> Generator [FileDraft]
genOperations spec =
  sequence
    [ genQueryTypesFile spec,
      genActionTypesFile spec,
      genQueriesIndex spec,
      genActionsIndex spec
    ]

genQueriesIndex :: AppSpec -> Generator FileDraft
genQueriesIndex spec = return $ mkTmplFdWithData relPath tmplData
  where
    relPath = [relfile|server/queries/index.ts|]
    tmplData =
      object
        [ "operations" .= map getQueryData (AS.getQueries spec)
        ]

genActionsIndex :: AppSpec -> Generator FileDraft
genActionsIndex spec = return $ mkTmplFdWithData relPath tmplData
  where
    relPath = [relfile|server/actions/index.ts|]
    tmplData =
      object
        [ "operations" .= map getActionData (AS.getActions spec)
        ]

genQueryTypesFile :: AppSpec -> Generator FileDraft
genQueryTypesFile spec = genOperationTypesFile tmplFile dstFile operations isAuthEnabledGlobally
  where
    tmplFile = [relfile|server/queries/types.ts|]
    dstFile = [relfile|server/queries/types.ts|]
    operations = map (uncurry AS.Operation.QueryOp) $ AS.getQueries spec
    isAuthEnabledGlobally = isAuthEnabled spec

genActionTypesFile :: AppSpec -> Generator FileDraft
genActionTypesFile spec = genOperationTypesFile tmplFile dstFile operations isAuthEnabledGlobally
  where
    tmplFile = [relfile|server/actions/types.ts|]
    dstFile = [relfile|server/actions/types.ts|]
    operations = map (uncurry AS.Operation.ActionOp) $ AS.getActions spec
    isAuthEnabledGlobally = isAuthEnabled spec

-- | Here we generate JS file that basically imports JS query function provided by user,
--   decorates it (mostly injects stuff into it) and exports. Idea is that the rest of the server,
--   and user also, should use this new JS function, and not the old one directly.
getQueryData :: (String, AS.Query.Query) -> Aeson.Value
getQueryData (queryName, query) = getOperationTmplData operation
  where
    operation = AS.Operation.QueryOp queryName query

getActionData :: (String, AS.Action.Action) -> Aeson.Value
getActionData (actionName, action) = getOperationTmplData operation
  where
    operation = AS.Operation.ActionOp actionName action

genOperationTypesFile ::
  Path' (Rel C.SdkTemplatesDir) File' ->
  Path' (Rel C.SdkRootDir) File' ->
  [AS.Operation.Operation] ->
  Bool ->
  Generator FileDraft
genOperationTypesFile tmplFile dstFile operations isAuthEnabledGlobally =
  return $ C.mkTmplFdWithDstAndData tmplFile dstFile (Just tmplData)
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
serverOperationsDirInSdkRootDir (AS.Operation.QueryOp _ _) = [reldir|server/queries|]
serverOperationsDirInSdkRootDir (AS.Operation.ActionOp _ _) = [reldir|server/actions|]

getOperationTmplData :: AS.Operation.Operation -> Aeson.Value
getOperationTmplData operation =
  object
    [ "jsFn" .= extOperationImportToImportJson (AS.Operation.getFn operation),
      "operationName" .= getName operation,
      "operationTypeName" .= toUpperFirst (getName operation),
      "entities"
        .= maybe [] (map (makeJsonWithEntityData . AS.refName)) (AS.Operation.getEntities operation)
    ]

extOperationImportToImportJson :: EI.ExtImport -> Aeson.Value
extOperationImportToImportJson =
  GJI.jsImportToImportJson
    . Just
    . applyExtImportAlias
    . extImportToJsImport

applyExtImportAlias :: JsImport -> JsImport
applyExtImportAlias jsImport =
  jsImport {_importAlias = Just $ JI.getImportIdentifier jsImport ++ "_ext"}

extImportToJsImport :: EI.ExtImport -> JsImport
extImportToJsImport extImport@(EI.ExtImport extImportName extImportPath) =
  JsImport
    { _path = ModuleImportPath importPath,
      _name = importName,
      _importAlias = Just $ EI.importIdentifier extImport ++ "_ext"
    }
  where
    importPath = C.makeSdkImportPath $ extCodeDirP </> SP.castRel extImportPath
    extCodeDirP = fromJust $ SP.relDirToPosix C.extCodeDirInSdkRootDir
    importName = GJI.extImportNameToJsImportName extImportName

-- extImportToImportJson :: EI.ExtImport -> Aeson.Value
-- extImportToImportJson extImport@(EI.ExtImport importName importPath) =
--   object
--     [ "isDefined" .= True,
--       "importStatement" .= Debug.trace jsImportStmt jsImportStmt,
--       "importIdentifier" .= importAlias
--     ]
--   where
--     jsImportStmt = case importName of
--       EI.ExtImportModule n -> "import " ++ n ++ " from '" ++ importPathStr ++ "'"
--       EI.ExtImportField n -> "import { " ++ n ++ " as " ++ importAlias ++ " } from '" ++ importPathStr ++ "'"
--     importPathStr = C.makeSdkImportPath $ extCodeDirP </> SP.castRel importPath
--     extCodeDirP = fromJust $ SP.relDirToPosix C.extCodeDirInSdkRootDir
-- importAlias = EI.importIdentifier extImport ++ "User"
