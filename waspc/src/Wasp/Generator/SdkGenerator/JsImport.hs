module Wasp.Generator.SdkGenerator.JsImport
  ( extImportToImportJson,
    extImportToAliasedImportJson,
  )
where

import qualified Data.Aeson as Aeson
import qualified Wasp.AppSpec.ExtImport as EI
import qualified Wasp.Generator.JsImport as GJI
import qualified Wasp.Generator.SdkGenerator.VirtualUserModules as VUM
import Wasp.JsImport (JsImport (..), JsImportKind (ValueImport), applyJsImportAlias)

extImportToImportJson :: Maybe EI.ExtImport -> Aeson.Value
extImportToImportJson maybeExtImport = GJI.jsImportToImportJson jsImport
  where
    jsImport = extImportToJsImport <$> maybeExtImport

-- | Like 'extImportToImportJson', but under a caller-chosen local alias.
-- Needed when one generated module imports several user values that may share
-- an export name (e.g. each auth provider's `userSignupFields`) -- the default
-- export-name-derived identifiers would collide.
extImportToAliasedImportJson :: String -> Maybe EI.ExtImport -> Aeson.Value
extImportToAliasedImportJson alias maybeExtImport = GJI.jsImportToImportJson jsImport
  where
    jsImport = applyJsImportAlias (Just alias) . extImportToJsImport <$> maybeExtImport

extImportToJsImport ::
  EI.ExtImport ->
  JsImport
extImportToJsImport extImport@(EI.ExtImport extImportName extImportPath _) =
  JsImport
    { _kind = ValueImport,
      _path = importPath,
      _name = importName,
      _importAlias = Just $ GJI.getAliasedExtImportIdentifier extImport
    }
  where
    importName = GJI.extImportNameToJsImportName extImportName
    importPath = VUM.extImportToVirtualUserModuleJsImportPath extImportPath
