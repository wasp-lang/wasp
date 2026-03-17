module Wasp.Generator.ServerGenerator.JsImport where

import qualified Data.Aeson as Aeson
import Data.Maybe (fromJust)
import StrongPath (Dir, Path, Posix, Rel, castDir, relDirToPosix, reldir, (</>))
import qualified Wasp.AppSpec.ExtImport as EI
import qualified Wasp.Generator.JsImport as GJI
import Wasp.Generator.ServerGenerator.Common (ServerSrcDir, serverSrcDirInProjectRootDir)
import Wasp.JsImport
  ( JsImport,
    JsImportAlias,
    JsImportIdentifier,
    JsImportStatement,
  )
import qualified Wasp.JsImport as JI
import Wasp.Project.Common (generatedCodeDirInWaspProjectDir, srcDirInWaspProjectDir)
import Wasp.Util.StrongPath (invertRelDir)

extImportToImportJson ::
  Path Posix (Rel importLocation) (Dir ServerSrcDir) ->
  Maybe EI.ExtImport ->
  Aeson.Value
extImportToImportJson pathFromImportLocationToSrcDir maybeExtImport = GJI.jsImportToImportJson jsImport
  where
    jsImport = extImportToJsImport pathFromImportLocationToSrcDir <$> maybeExtImport

extImportToAliasedImportJson ::
  JsImportAlias ->
  Path Posix (Rel importLocation) (Dir ServerSrcDir) ->
  Maybe EI.ExtImport ->
  Aeson.Value
extImportToAliasedImportJson importAlias pathFromImportLocationToSrcDir maybeExtImport = GJI.jsImportToImportJson aliasedJsImport
  where
    jsImport = extImportToJsImport pathFromImportLocationToSrcDir <$> maybeExtImport
    aliasedJsImport = JI.applyJsImportAlias (Just importAlias) <$> jsImport

getJsImportStmtAndIdentifier ::
  Path Posix (Rel importLocation) (Dir ServerSrcDir) ->
  EI.ExtImport ->
  (JsImportStatement, JsImportIdentifier)
getJsImportStmtAndIdentifier pathFromImportLocationToExtCodeDir = JI.getJsImportStmtAndIdentifier . extImportToJsImport pathFromImportLocationToExtCodeDir

getAliasedJsImportStmtAndIdentifier ::
  JsImportAlias ->
  Path Posix (Rel importLocation) (Dir ServerSrcDir) ->
  EI.ExtImport ->
  (JsImportStatement, JsImportIdentifier)
getAliasedJsImportStmtAndIdentifier importAlias pathFromImportLocationToExtCodeDir =
  JI.getJsImportStmtAndIdentifier . JI.applyJsImportAlias (Just importAlias) . extImportToJsImport pathFromImportLocationToExtCodeDir

-- NOTE: We have to cast dir because the 'GJI' expects path to the `sdk/wasp/src` dir.
extImportToJsImport ::
  Path Posix (Rel importLocation) (Dir ServerSrcDir) ->
  EI.ExtImport ->
  JsImport
extImportToJsImport = GJI.extImportToJsImport $ fromJust . relDirToPosix $ castDir waspProjectSrcDirFromServerSrcDir
  where
    -- NOTE: Instead of generating the `src` folder with the user's code and
    -- referencing that, we reference user code directly. This gives us proper
    -- error messages (with user's file names and line numbers). It works great
    -- with Vite (Vite outputs absolute file paths), but less great on the
    -- server (TS outputs relative paths, resulting in ../../src/something).
    waspProjectSrcDirFromServerSrcDir = waspProjectDirFromServerSrcDir </> srcDirInWaspProjectDir
    waspProjectDirFromServerSrcDir = invertRelDir (generatedCodeDirInWaspProjectDir </> serverSrcDirInProjectRootDir)
