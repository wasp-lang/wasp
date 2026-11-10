module Wasp.Docs
  ( waspDocsUrl,
  )
where

import qualified Wasp.SemanticVersion as SV
import Wasp.Version (waspVersion)

-- | The URL of a page in the docs of this Wasp version, e.g.
-- @waspDocsUrl "advanced/dependencies#overriding"@.
waspDocsUrl :: String -> String
waspDocsUrl pagePath = "https://wasp.sh/docs/" ++ docsVersion ++ "/" ++ pagePath
  where
    -- The docs have one version per Wasp minor version, e.g. `0.27`.
    docsVersion = show (SV.major waspVersion) ++ "." ++ show (SV.minor waspVersion)
