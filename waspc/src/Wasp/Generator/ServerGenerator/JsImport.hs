module Wasp.Generator.ServerGenerator.JsImport where

import qualified Data.Aeson as Aeson
import StrongPath (Dir, Path, Posix, Rel)
import StrongPath.TH (reldirP)
import qualified Wasp.AppSpec.ExtImport as EI
import qualified Wasp.Generator.JsImport as GJI
import Wasp.Generator.ServerGenerator.Common (ServerSrcDir)
import Wasp.JsImport
  ( JsImport,
    JsImportAlias,
    JsImportIdentifier,
    JsImportStatement,
  )
import qualified Wasp.JsImport as JI

extImportToImportJson ::
  Path Posix (Rel importLocation) (Dir ServerSrcDir) ->
  Maybe EI.ExtImport ->
  Aeson.Value
extImportToImportJson pathFromImportLocationToSrcDir maybeExtImport = GJI.jsImportToImportJson jsImport
  where
    jsImport = extImportToJsImport pathFromImportLocationToSrcDir <$> maybeExtImport

aliasedExtImportToImportJson ::
  JsImportAlias ->
  Path Posix (Rel importLocation) (Dir ServerSrcDir) ->
  Maybe EI.ExtImport ->
  Aeson.Value
aliasedExtImportToImportJson importAlias pathFromImportLocationToSrcDir maybeExtImport = GJI.jsImportToImportJson aliasedJsImport
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

extImportToJsImport ::
  Path Posix (Rel importLocation) (Dir ServerSrcDir) ->
  EI.ExtImport ->
  JsImport
extImportToJsImport = GJI.extImportToJsImport serverExtDir
  where
    -- filip: Instead of generating the ext-src folder with the user's code and referencing that, we reference user code directly.
    -- This gives us proper error messages (with user's file names and line numbers).
    -- It works great with Vite (Vite outputs absolute file paths), but less great on the server (TS outputs relative paths, resulting in ../../src/something)
    serverExtDir = [reldirP|../../../../src|]
