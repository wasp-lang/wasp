module Wasp.PackageManager.Detection.UserAgent
  ( detectFromUserAgent,

    -- * Exported for testing only
    parseUserAgent,
  )
where

import System.Environment (lookupEnv)
import Wasp.PackageManager.Detection.Common (Detection (..), DetectionSource (..))
import Wasp.PackageManager.PackageManager (PackageManager, parsePackageManagerName)

-- | The package manager that launched Wasp, if any.
detectFromUserAgent :: IO (Maybe Detection)
detectFromUserAgent =
  fmap (`Detection` UserAgent) . (>>= parseUserAgent) <$> lookupEnv userAgentEnvVarName

-- | Package managers set this for every process they spawn, such as package
-- scripts, @npx@, @pnpm dlx@ and @npm create@.
userAgentEnvVarName :: String
userAgentEnvVarName = "npm_config_user_agent"

-- | Reads the first product of a user agent like
-- @pnpm/9.13.2 npm/? node/v22.11.0 darwin arm64@.
parseUserAgent :: String -> Maybe PackageManager
parseUserAgent userAgent = case words userAgent of
  firstProduct : _ -> parsePackageManagerName $ takeWhile (/= '/') firstProduct
  [] -> Nothing
