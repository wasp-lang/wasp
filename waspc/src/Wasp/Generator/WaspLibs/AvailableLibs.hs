module Wasp.Generator.WaspLibs.AvailableLibs
  ( waspLibs,
  )
where

import StrongPath (reldir)
import qualified Wasp.Generator.WaspLibs.WaspLib as WaspLib

waspLibs :: [WaspLib.WaspLib]
waspLibs =
  [ -- NOTE: The package names of the libs should match the names in the
    -- `package.json` files of the libs in the <repo>/packages/libs directory.
    WaspLib.makeWaspLib "@wasp/lib-auth" [reldir|auth|],
    WaspLib.makeWaspLib "@wasp/lib-sdk-core" [reldir|sdk-core|],
    WaspLib.makeWaspLib "@wasp/lib-test-utils" [reldir|test-utils|],
    WaspLib.makeWaspLib "@wasp/lib-vite-ssr" [reldir|vite-ssr|]
  ]
