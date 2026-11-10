module Wasp.Docs
  ( versionedWaspDocsUrl,
  )
where

import qualified Wasp.SemanticVersion as SV
import Wasp.Version (waspVersion)

-- | The URL of the docs of the latest Wasp version.
waspDocsUrl :: String
waspDocsUrl = "https://wasp.sh/docs"

-- | The URL of a page in the docs of this Wasp version, e.g.
-- @versionedWaspDocsUrl "advanced/dependencies#overriding"@.
versionedWaspDocsUrl :: String -> String
versionedWaspDocsUrl pagePath = waspDocsUrl ++ "/" ++ docsVersion ++ "/" ++ pagePath
  where
    -- The docs have one version per Wasp minor version, e.g. `0.27`.
    docsVersion = show (SV.major waspVersion) ++ "." ++ show (SV.minor waspVersion)
