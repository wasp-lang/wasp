-- | A central place to define the npm dependency versions of generated Wasp
-- apps. Versions are read at compile time, with @$$(catalogVersionRange
-- "react")@. A missing package or an invalid range fails the compilation of
-- Wasp.
--
-- They are used in multiple places (also the supporting libraries), so we
-- declare them in the root `pnpm-workspace.yaml` file, and read them from
-- there. This means that supporting libraries can also use them, and everything
-- stays in sync.
--
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

-- | The version range of the given npm package in the catalog.
catalogVersionRange :: String -> TH.Code TH.Q SV.Range
catalogVersionRange packageName = TH.joinCode $ TH.liftTyped <$> readVersionRange packageName

readVersionRange :: String -> TH.Q SV.Range
readVersionRange packageName = do
  catalog <- readCatalog
  versionRange <-
    maybe
      (fail $ "Package " ++ show packageName ++ " not found in the catalog of generated app dependency versions")
      pure
      (M.lookup packageName catalog)
  either
    (\err -> fail $ "Invalid version range for " ++ show packageName ++ " in the catalog of generated app dependency versions: " ++ show err)
    pure
    (SV.parseRange versionRange)

-- | We store the catalog as the @generated-app@ named pnpm catalog, in the
-- monorepo's @pnpm-workspace.yaml@. That way our JS packages can use the same
-- versions, by depending on @catalog:generated-app@.
--
-- We read that file through the @pnpm-workspace.link.yaml@ symlink because Cabal
-- warns about depending on files outside of the package (see @waspc.cabal@).
readCatalog :: TH.Q (M.Map String String)
readCatalog = do
  pnpmWorkspaceFile <- TH.makeRelativeToProject "pnpm-workspace.link.yaml"
  TH.addDependentFile pnpmWorkspaceFile
  TH.runIO $ generatedAppCatalog <$> Yaml.decodeFileThrow pnpmWorkspaceFile

newtype PnpmWorkspace = PnpmWorkspace {generatedAppCatalog :: M.Map String String}

instance Aeson.FromJSON PnpmWorkspace where
  parseJSON = Aeson.withObject "PnpmWorkspace" $ \workspace ->
    PnpmWorkspace <$> (workspace .: "catalogs" >>= (.: "generated-app"))
