module Wasp.Generator.WaspLibs.AvailableLibs
  ( waspLibs,
  )
where

import StrongPath (reldir)
import qualified Wasp.Generator.WaspLibs.WaspLib as WaspLib

waspLibs :: [WaspLib.WaspLib]
waspLibs =
  [ -- NOTE: The package names of the libs should match the names in the
    -- `package.json` files of the libs in the ./data/Generator/libs directory.
    -- NOTE: `@wasp.sh/lib-vite-ssr` used to be listed here, but Nitro took over
    -- serving and prerendering the client. Its source directory is still around
    -- and will be deleted separately.
    WaspLib.makeWaspLib "@wasp.sh/lib-auth" [reldir|auth|]
  ]
