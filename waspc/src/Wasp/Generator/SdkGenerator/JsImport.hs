module Wasp.Generator.SdkGenerator.JsImport
  ( extOperationImportToImportJson,
    extImportToImportJson,
    extImportToJsImport,
  )
where

import qualified Data.Aeson as Aeson
import Data.Maybe (fromJust)
import StrongPath (Dir, Path, Posix, Rel, relDirToPosix, (</>))
import qualified Wasp.AppSpec.ExtImport as EI
import qualified Wasp.Generator.JsImport as GJI
import Wasp.Generator.SdkGenerator.Common (SdkSrcDir, sdkRootDirInGeneratedAppDir, sdkSrcDirInSdkRootDir)
import Wasp.JsImport (JsImport (..))
import Wasp.Project.Common (generatedAppDirInWaspProjectDir, srcDirInWaspProjectDir)
import Wasp.Util.StrongPath (invertRelDir)

extImportToImportJson ::
  Path Posix (Rel importLocation) (Dir SdkSrcDir) ->
  Maybe EI.ExtImport ->
  Aeson.Value
extImportToImportJson pathFromImportLocationToSrcDir maybeExtImport = GJI.jsImportToImportJson jsImport
  where
    jsImport = extImportToJsImport pathFromImportLocationToSrcDir <$> maybeExtImport

extOperationImportToImportJson ::
  Path Posix (Rel importLocation) (Dir SdkSrcDir) ->
  EI.ExtImport ->
  Aeson.Value
extOperationImportToImportJson pathFromImportLocationToSrcDir =
  GJI.jsImportToImportJson
    . Just
    . extImportToJsImport pathFromImportLocationToSrcDir

extImportToJsImport ::
  Path Posix (Rel importLocation) (Dir SdkSrcDir) ->
  EI.ExtImport ->
  JsImport
extImportToJsImport = GJI.extImportToJsImport $ fromJust . relDirToPosix $ waspProjectSrcDirFromSdkSrcDir
  where
    waspProjectSrcDirFromSdkSrcDir = waspProjectDirFromSdkSrcDir </> srcDirInWaspProjectDir
    waspProjectDirFromSdkSrcDir = invertRelDir (generatedAppDirInWaspProjectDir </> sdkRootDirInGeneratedAppDir </> sdkSrcDirInSdkRootDir)
