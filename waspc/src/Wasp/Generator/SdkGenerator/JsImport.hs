module Wasp.Generator.SdkGenerator.JsImport
  ( extOperationImportToImportJson,
    extImportToImportJson,
    extImportToJsImport,
  )
where

import qualified Data.Aeson as Aeson
import Data.Maybe (fromJust)
import StrongPath (Dir, File', Path, Path', Posix, Rel, castRel, relDirToPosix, (</>))
import qualified StrongPath as SP
import qualified Wasp.AppSpec.ExtImport as EI
import Wasp.Generator.ExternalCodeGenerator.Common (GeneratedExternalCodeDir)
import Wasp.Generator.JsImport (getAliasedExtImportIdentifier)
import qualified Wasp.Generator.JsImport as GJI
import Wasp.Generator.SdkGenerator.Common (SdkRootDir, extSrcDirInSdkRootDir)
import Wasp.JsImport (JsImport (..), JsImportKind (ValueImport), JsImportPath (..))
import Wasp.Util.StrongPath (invertRelDir)

extImportToImportJson ::
  -- | The directory of the importing file, relative to the SDK root.
  Path' (Rel SdkRootDir) (Dir importLocation) ->
  Maybe EI.ExtImport ->
  Aeson.Value
extImportToImportJson importLocationDirInSdkRootDir maybeExtImport =
  GJI.jsImportToImportJson $ extImportToJsImport importLocationDirInSdkRootDir <$> maybeExtImport

extOperationImportToImportJson ::
  -- | The directory of the importing file, relative to the SDK root.
  Path' (Rel SdkRootDir) (Dir importLocation) ->
  EI.ExtImport ->
  Aeson.Value
extOperationImportToImportJson importLocationDirInSdkRootDir =
  GJI.jsImportToImportJson . Just . extImportToJsImport importLocationDirInSdkRootDir

-- | Builds a JS import of user (external) code copied into the SDK. We import it
-- relative to the importing file (rather than via the `wasp/src/...` self-import)
-- so that a `.ts` extension in the import path can be rewritten to `.js` on emit
-- (`rewriteRelativeImportExtensions` only rewrites relative specifiers).
extImportToJsImport ::
  -- | The directory of the importing file, relative to the SDK root.
  Path' (Rel SdkRootDir) (Dir importLocation) ->
  EI.ExtImport ->
  JsImport
extImportToJsImport importLocationDirInSdkRootDir extImport@(EI.ExtImport extImportName extImportPath _) =
  JsImport
    { _kind = ValueImport,
      _path = RelativeImportPath importPath,
      _name = importName,
      _importAlias = Just $ getAliasedExtImportIdentifier extImport
    }
  where
    importName = GJI.extImportNameToJsImportName extImportName
    importPath =
      SP.castRel $
        pathFromImportLocationToSdkRootDir </> extSrcDirInSdkRootDirP </> userDefinedPathInExtSrcDir
    pathFromImportLocationToSdkRootDir = fromJust $ relDirToPosix $ invertRelDir importLocationDirInSdkRootDir
    extSrcDirInSdkRootDirP = fromJust $ relDirToPosix extSrcDirInSdkRootDir
    userDefinedPathInExtSrcDir = castRel extImportPath :: Path Posix (Rel GeneratedExternalCodeDir) File'
