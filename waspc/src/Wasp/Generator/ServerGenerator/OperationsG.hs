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
import Data.Char (toLower)
import Data.Maybe (fromJust)
import StrongPath (Dir, Dir', File', Path, Path', Posix, Rel, reldir, reldirP, relfile, (</>))
import qualified StrongPath as SP
import Wasp.AppSpec (AppSpec)
import qualified Wasp.AppSpec as AS
import qualified Wasp.AppSpec.Action as AS.Action
import qualified Wasp.AppSpec.Operation as AS.Operation
import qualified Wasp.AppSpec.Query as AS.Query
import Wasp.Generator.ExternalCodeGenerator.Common (GeneratedExternalCodeDir)
import Wasp.Generator.FileDraft (FileDraft)
import Wasp.Generator.JsImport (getJsImportDetailsForExtFnImport)
import qualified Wasp.Generator.ServerGenerator.Common as C

genOperations :: AppSpec -> [FileDraft]
genOperations spec = genQueries spec ++ genActions spec

genQueries :: AppSpec -> [FileDraft]
genQueries spec = map (genQuery spec) (AS.getQueries spec)

genActions :: AppSpec -> [FileDraft]
genActions spec = map (genAction spec) (AS.getActions spec)

-- | Here we generate JS file that basically imports JS query function provided by user,
--   decorates it (mostly injects stuff into it) and exports. Idea is that the rest of the server,
--   and user also, should use this new JS function, and not the old one directly.
genQuery :: AppSpec -> (String, AS.Query.Query) -> FileDraft
genQuery _ (queryName, query) = C.mkTmplFdWithDstAndData tmplFile dstFile (Just tmplData)
  where
    operation = AS.Operation.QueryOp queryName query
    tmplFile = C.asTmplFile [relfile|src/queries/_query.js|]
    dstFile = C.serverSrcDirInServerRootDir </> queryFileInSrcDir queryName
    tmplData = operationTmplData operation

-- | Analogous to genQuery.
genAction :: AppSpec -> (String, AS.Action.Action) -> FileDraft
genAction _ (actionName, action) = C.mkTmplFdWithDstAndData tmplFile dstFile (Just tmplData)
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

-- | TODO: Make this not hardcoded!
relPosixPathFromOperationFileToExtSrcDir :: Path Posix (Rel Dir') (Dir GeneratedExternalCodeDir)
relPosixPathFromOperationFileToExtSrcDir = [reldirP|../ext-src/|]

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
    buildEntityData :: String -> Aeson.Value
    buildEntityData entityName =
      object
        [ "name" .= entityName,
          "prismaIdentifier" .= (toLower (head entityName) : tail entityName)
        ]
