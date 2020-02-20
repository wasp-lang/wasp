module Generator.Common
    ( srcDirPath
    ) where

import Path (reldir)
import Path.Aliases as Path

-- | Path to src directory, relative to the root directory of generated code.
srcDirPath :: Path.RelDir
srcDirPath = [reldir|src|]
