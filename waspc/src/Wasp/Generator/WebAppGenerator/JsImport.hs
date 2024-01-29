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
    -- filip: read notes in ServerGenerator/JsImport.hs
    -- todo(filip): use WaspProjectDirInProjectRootDir (once you add it for
    -- Prisma stuff) and other stuff from WebAppGenerator/Common to build this
    -- directory. Do the same for the server
    webAppExtDir = [reldirP|../../../../src|]
