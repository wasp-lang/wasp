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
import Wasp.AppSpec.Valid (Valid (Valid), (<$^^>))
import Wasp.Generator.ExternalCodeGenerator.Common (GeneratedExternalCodeDir)
import Wasp.Generator.FileDraft (FileDraft)
import Wasp.Generator.JsImport (getJsImportDetailsForExtFnImport)
import Wasp.Generator.Monad (Generator)
import qualified Wasp.Generator.ServerGenerator.Common as C
import Wasp.Util ((<++>))

genOperations :: Valid AppSpec -> Generator [FileDraft]
genOperations spec = genQueries spec <++> genActions spec

genQueries :: Valid AppSpec -> Generator [FileDraft]
genQueries spec = mapM (genQuery spec) (AS.getQueries <$^^> spec)

genActions :: Valid AppSpec -> Generator [FileDraft]
genActions spec = mapM (genAction spec) (AS.getActions <$^^> spec)

-- | Here we generate JS file that basically imports JS query function provided by user,
--   decorates it (mostly injects stuff into it) and exports. Idea is that the rest of the server,
--   and user also, should use this new JS function, and not the old one directly.
genQuery :: Valid AppSpec -> (String, Valid AS.Query.Query) -> Generator FileDraft
genQuery _ (queryName, Valid query) = return $ C.mkTmplFdWithDstAndData tmplFile dstFile (Just tmplData)
  where
    operation = AS.Operation.QueryOp queryName query
    tmplFile = C.asTmplFile [relfile|src/queries/_query.js|]
    dstFile = C.serverSrcDirInServerRootDir </> queryFileInSrcDir queryName
    tmplData = operationTmplData operation

-- | Analogous to genQuery.
genAction :: Valid AppSpec -> (String, Valid AS.Action.Action) -> Generator FileDraft
genAction _ (actionName, Valid action) = return $ C.mkTmplFdWithDstAndData tmplFile dstFile (Just tmplData)
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
