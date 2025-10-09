module Wasp.Node.Commands
  ( -- * Node.js Commands
    -- These are the resolved commands for Node.js executables (node, npm, npx).
    -- On Windows, these will include the proper extensions (.cmd, .exe, etc.).
    -- On other platforms, these are just the base names.
    nodeCmd,
    npmCmd,
    npxCmd,
  )
where

import Wasp.Util.Process (resolveExecutable)

-- | The resolved command for the node executable.
-- On Windows, this might be "node.exe" or a full path.
-- On Unix, this is just "node".
nodeCmd :: String
nodeCmd = resolveExecutable "node"

-- | The resolved command for the npm executable.
-- On Windows, this might be "npm.cmd" or a full path.
-- On Unix, this is just "npm".
npmCmd :: String
npmCmd = resolveExecutable "npm"

-- | The resolved command for the npx executable.
-- On Windows, this might be "npx.cmd" or a full path.
-- On Unix, this is just "npx".
npxCmd :: String
npxCmd = resolveExecutable "npx"
