module Wasp.Generator.WebAppGenerator.JsImport where

import qualified Data.Aeson as Aeson
import StrongPath (Dir, Path, Posix, Rel)
import StrongPath.TH (reldirP)
import Wasp.AppSpec.ExtImport (ExtImport (..))
import qualified Wasp.AppSpec.ExtImport as EI
import qualified Wasp.Generator.JsImport as GJI
import Wasp.Generator.WebAppGenerator.Common (WebAppSrcDir)
import Wasp.JsImport
  ( JsImport,
  )

extImportToImportJson ::
  Path Posix (Rel importLocation) (Dir WebAppSrcDir) ->
  Maybe ExtImport ->
  Aeson.Value
extImportToImportJson pathFromImportLocationToSrcDir maybeExtImport = GJI.jsImportToImportJson jsImport
  where
    jsImport = extImportToJsImport pathFromImportLocationToSrcDir <$> maybeExtImport

extImportToJsImport ::
  Path Posix (Rel importLocation) (Dir WebAppSrcDir) ->
  EI.ExtImport ->
  JsImport
extImportToJsImport = GJI.extImportToJsImport webAppExtDir
  where
    -- NOTE: read the  notes in ServerGenerator/JsImport.hs
    webAppExtDir = [reldirP|../../../../src|]
