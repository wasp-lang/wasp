module Wasp.PackageManager.UserAgent
  ( parseUserAgent,
    userAgentEnvVarName,
  )
where

import Wasp.PackageManager.PackageManager (PackageManager, parsePackageManagerName)

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
