module Wasp.Node.NodeModules
  ( getPathToExecutableInNodeModules,
  )
where

import Data.Maybe (fromJust)
import StrongPath (File, Path', Rel, parseRelFile, reldir, (</>))
import Wasp.Util.System (isSystemWindows)

-- | Represents some node_modules dir.
data NodeModulesDir

-- | Node modules (node_modules) have a place where they put all the executables/binaries
-- produced by the packages/modules.
-- This function returns a path to such an executable with a given name, taking into account
-- details like current operating system.
--
-- Example: @getPathToExecutableInNodeModules "npm"@ -> @".bin/npm.cmd"@
getPathToExecutableInNodeModules :: String -> Path' (Rel NodeModulesDir) (File f)
getPathToExecutableInNodeModules execName =
  [reldir|.bin|] </> fromJust (parseRelFile systemSpecificExecFilename)
  where
    systemSpecificExecFilename
      | isSystemWindows = execName <> ".cmd"
      | otherwise = execName
