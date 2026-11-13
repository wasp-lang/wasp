module Wasp.Generator.WaspLibs.AvailableLibs
  ( waspLibs,
    getWaspLibsForApp,
  )
where

import StrongPath (reldir)
import Wasp.AppSpec (AppSpec)
import Wasp.AppSpec.Valid (isMcpEnabled)
import qualified Wasp.Generator.WaspLibs.WaspLib as WaspLib

-- | Every lib Wasp ships, whether or not an app uses it.
waspLibs :: [WaspLib.WaspLib]
waspLibs =
  [ -- NOTE: The package names of the libs should match the names in the
    -- `package.json` files of the libs in the <repo>/packages/libs directory.
    WaspLib.makeWaspLib "@wasp/lib-auth" [reldir|auth|],
    WaspLib.makeWaspLib "@wasp/lib-sdk-core" [reldir|sdk-core|],
    WaspLib.makeWaspLib "@wasp/lib-test-utils" [reldir|test-utils|],
    WaspLib.makeWaspLib "@wasp/lib-vite-ssr" [reldir|vite-ssr|],
    mcpLib
  ]

-- | The libs an app gets. Feature libs are included only when the app uses
-- the feature, so apps without it don't install their dependencies.
getWaspLibsForApp :: AppSpec -> [WaspLib.WaspLib]
getWaspLibsForApp spec = filter isUsed waspLibs
  where
    isUsed lib
      | WaspLib.packageName lib == WaspLib.packageName mcpLib = isMcpEnabled spec
      | otherwise = True

mcpLib :: WaspLib.WaspLib
mcpLib = WaspLib.makeWaspLib "@wasp/lib-mcp" [reldir|mcp|]
