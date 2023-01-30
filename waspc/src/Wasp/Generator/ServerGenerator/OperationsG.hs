{-# LANGUAGE TypeApplications #-}

module Wasp.Generator.ServerGenerator.OperationsG
  ( genOperations,
    queryFileInSrcDir,
    actionFileInSrcDir,
    operationFileInSrcDir,
  )
where

import Data.Aeson (object, (.=))
import qualified Data.Aeson as Aeson
import Data.List (nub)
import Data.Maybe (fromJust, fromMaybe)
import StrongPath (Dir, Dir', File', Path, Path', Posix, Rel, relDirToPosix, reldir, reldirP, relfile, (</>))
import qualified StrongPath as SP
import Wasp.AppSpec (AppSpec)
import qualified Wasp.AppSpec as AS
import qualified Wasp.AppSpec.Action as AS.Action
import Wasp.AppSpec.Operation (getName)
import qualified Wasp.AppSpec.Operation as AS.Operation
import qualified Wasp.AppSpec.Query as AS.Query
import Wasp.Generator.Common (buildEntityData)
import Wasp.AppSpec.Valid (isAuthEnabled)
import Wasp.Generator.ExternalCodeGenerator.Common (GeneratedExternalCodeDir)
import Wasp.Generator.FileDraft (FileDraft)
import Wasp.Generator.JsImport (getJsImportDetailsForExtFnImport)
import Wasp.Generator.Monad (Generator)
import qualified Wasp.Generator.ServerGenerator.Common as C
import Wasp.Generator.ServerGenerator.ExternalCodeGenerator (extServerCodeDirInServerSrcDir)
import Wasp.Util (toUpperFirst, (<++>))

genOperations :: AppSpec -> Generator [FileDraft]
genOperations spec =
  sequence
    [ genQueryTypesFile spec,
      genActionTypesFile spec
    ]
    <++> genQueries spec
    <++> genActions spec

genQueries :: AppSpec -> Generator [FileDraft]
genQueries = mapM genQuery . AS.getQueries

genActions :: AppSpec -> Generator [FileDraft]
genActions = mapM genAction . AS.getActions

genQueryTypesFile :: AppSpec -> Generator FileDraft
genQueryTypesFile spec = genOperationTypesFile tmplFile dstFile operations isAuthEnabledGlobally
  where
    tmplFile = [relfile|src/queries/types.ts|]
    dstFile = [relfile|src/queries/types.ts|]
    operations = map (uncurry AS.Operation.QueryOp) $ AS.getQueries spec
    isAuthEnabledGlobally = isAuthEnabled spec

genActionTypesFile :: AppSpec -> Generator FileDraft
genActionTypesFile spec = genOperationTypesFile tmplFile dstFile operations isAuthEnabledGlobally
  where
    tmplFile = [relfile|src/actions/types.ts|]
    dstFile = [relfile|src/actions/types.ts|]
    operations = map (uncurry AS.Operation.ActionOp) $ AS.getActions spec
    isAuthEnabledGlobally = isAuthEnabled spec

-- | Here we generate JS file that basically imports JS query function provided by user,
--   decorates it (mostly injects stuff into it) and exports. Idea is that the rest of the server,
--   and user also, should use this new JS function, and not the old one directly.
genQuery :: (String, AS.Query.Query) -> Generator FileDraft
genQuery (queryName, query) = return $ C.mkTmplFdWithDstAndData tmplFile dstFile (Just tmplData)
  where
    operation = AS.Operation.QueryOp queryName query
    tmplFile = C.asTmplFile [relfile|src/queries/_query.js|]
    dstFile = C.serverSrcDirInServerRootDir </> queryFileInSrcDir queryName
    tmplData = operationTmplData operation

genOperationTypesFile ::
  Path' (Rel C.ServerTemplatesDir) File' ->
  Path' (Rel C.ServerRootDir) File' ->
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
          "allEntities" .= nub (concatMap entityNames operations)
        ]
    operationTypeData operation =
      object
        [ "typeName" .= toUpperFirst (getName operation),
          "entities" .= entityNames operation,
          "usesAuth" .= usesAuth operation
        ]
    entityNames = maybe [] (map AS.refName) . AS.Operation.getEntities
    usesAuth = fromMaybe isAuthEnabledGlobally . AS.Operation.getAuth

-- | Analogous to genQuery.
genAction :: (String, AS.Action.Action) -> Generator FileDraft
genAction (actionName, action) = return $ C.mkTmplFdWithDstAndData tmplFile dstFile (Just tmplData)
  where
    operation = AS.Operation.ActionOp actionName action
    tmplFile = [relfile|src/actions/_action.js|]
    dstFile = C.serverSrcDirInServerRootDir </> actionFileInSrcDir actionName
    tmplData = operationTmplData operation

queryFileInSrcDir :: String -> Path' (Rel C.ServerSrcDir) File'
queryFileInSrcDir queryName =
  [reldir|queries|]
    -- TODO: fromJust here could fail if there is some problem with the name, we should handle this.
    </> fromJust (SP.parseRelFile $ queryName ++ ".js")

actionFileInSrcDir :: String -> Path' (Rel C.ServerSrcDir) File'
actionFileInSrcDir actionName =
  [reldir|actions|]
    -- TODO: fromJust here could fail if there is some problem with the name, we should handle this.
    </> fromJust (SP.parseRelFile $ actionName ++ ".js")

operationFileInSrcDir :: AS.Operation.Operation -> Path' (Rel C.ServerSrcDir) File'
operationFileInSrcDir (AS.Operation.QueryOp name _) = queryFileInSrcDir name
operationFileInSrcDir (AS.Operation.ActionOp name _) = actionFileInSrcDir name

relPosixPathFromOperationFileToExtSrcDir :: Path Posix (Rel Dir') (Dir GeneratedExternalCodeDir)
relPosixPathFromOperationFileToExtSrcDir =
  [reldirP|../|] </> fromJust (relDirToPosix extServerCodeDirInServerSrcDir)

operationTmplData :: AS.Operation.Operation -> Aeson.Value
operationTmplData operation =
  object
    [ "jsFnImportStatement" .= importStmt,
      "jsFnIdentifier" .= importIdentifier,
      "entities" .= maybe [] (map (buildEntityData . AS.refName)) (AS.Operation.getEntities operation)
    ]
  where
    (importIdentifier, importStmt) =
      getJsImportDetailsForExtFnImport relPosixPathFromOperationFileToExtSrcDir $
        AS.Operation.getFn operation
