module Wasp.Generator.DepVersions.Catalog
  ( catalogVersionRange,
  )
where

import Data.Aeson ((.:))
import qualified Data.Aeson as Aeson
import qualified Data.Map as M
import qualified Data.Yaml as Yaml
import qualified Language.Haskell.TH.Syntax as TH
import qualified Wasp.SemanticVersion as SV

-- | Version range of an npm dependency of the generated app, read at compile
-- time from the pnpm catalog in the monorepo's `pnpm-workspace.yaml`
-- (through the `pnpm-workspace.link.yaml` symlink, see `waspc.cabal`).
-- Fails compilation if the package is missing from the catalog or its range is invalid.
--
-- Usage: @$$(catalogVersionRange "react")@
catalogVersionRange :: String -> TH.Code TH.Q SV.Range
catalogVersionRange packageName = TH.joinCode $ TH.liftTyped <$> readCatalogVersionRange packageName

readCatalogVersionRange :: String -> TH.Q SV.Range
readCatalogVersionRange packageName = do
  pnpmWorkspaceFile <- TH.makeRelativeToProject "pnpm-workspace.link.yaml"
  TH.addDependentFile pnpmWorkspaceFile
  catalog <- TH.runIO $ pnpmCatalog <$> Yaml.decodeFileThrow pnpmWorkspaceFile
  versionRange <-
    maybe
      (fail $ "Package " ++ show packageName ++ " not found in the pnpm catalog of " ++ pnpmWorkspaceFile)
      pure
      (M.lookup packageName catalog)
  either
    (\err -> fail $ "Invalid version range for " ++ show packageName ++ " in the pnpm catalog: " ++ show err)
    pure
    (SV.parseRange versionRange)

newtype PnpmWorkspace = PnpmWorkspace {pnpmCatalog :: M.Map String String}

instance Aeson.FromJSON PnpmWorkspace where
  parseJSON = Aeson.withObject "PnpmWorkspace" $ \workspace ->
    PnpmWorkspace <$> workspace .: "catalog"
