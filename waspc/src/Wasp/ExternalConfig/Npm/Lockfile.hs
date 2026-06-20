module Wasp.ExternalConfig.Npm.Lockfile
  ( stripStaleWaspEntries,
  )
where

import Control.Lens ((%~), (&))
import Data.Aeson (Value (..), KeyMap)
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import Data.Aeson.Lens (key, _Object)
import Data.Text (isPrefixOf)
import qualified Data.Text as T

-- | Removes stale Wasp-generated entries from a 'package-lock.json' 'Value'.
--
-- Mirrors the jq logic from the dev script's
-- @remove_stale_generated_wasp_lock_entries@. Call this before running
-- @npm install@ to prevent errors from old tarball references after a
-- Wasp version bump.
--
-- Removes:
--   1. Package entries whose keys start with stale prefixes such as
--      @"node_modules\/\@wasp.sh\/lib-"@ or @".wasp\/out"@.
--   2. @\@wasp.sh\/lib-*@ entries from remaining packages' @dependencies@.
stripStaleWaspEntries :: Value -> Value
stripStaleWaspEntries =
  (key "packages" . _Object %~ KM.filterWithKey (\k _ -> not (isStaleKey k)))
    . (key "packages" . _Object . traverse %~ cleanWaspLibDeps)

-- | From a single package entry, remove @\@wasp.sh\/lib-*@ entries
-- from its @"dependencies"@ object.
cleanWaspLibDeps :: Value -> Value
cleanWaspLibDeps (Object pkg) =
  Object $ KM.alter removeWaspLibDeps "dependencies" pkg
  where
    removeWaspLibDeps :: Maybe Value -> Maybe Value
    removeWaspLibDeps (Just (Object deps)) =
      let filtered = KM.filterWithKey (\k _ -> not (isWaspLibDepKey k)) deps
      in if KM.null filtered then Nothing else Just (Object filtered)
    removeWaspLibDeps x = x
cleanWaspLibDeps v = v

-- | Keys whose presence in @"packages"@ marks them as stale.
isStaleKey :: Key.Key -> Bool
isStaleKey k =
  let t = Key.toText k
  in any (`T.isPrefixOf` t) stalePrefixes

stalePrefixes :: [T.Text]
stalePrefixes =
  [ "node_modules/@wasp.sh/lib-",
    "node_modules/@wasp.sh/generated-server",
    "node_modules/wasp",
    ".wasp/out",
  ]

isWaspLibDepKey :: Key.Key -> Bool
isWaspLibDepKey k =
  let t = Key.toText k
  in "@wasp.sh/lib-" `T.isPrefixOf` t
