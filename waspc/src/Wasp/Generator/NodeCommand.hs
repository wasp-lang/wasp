module Wasp.Generator.NodeCommand
  ( NodeCommand (..),
    proc,
    toShellCommand,
    typeCheckUserCode,
  )
where

import StrongPath (Abs, Dir, File, Path', Rel)
import qualified StrongPath as SP
import qualified Wasp.Job as J
import Wasp.Project.Common (SrcTsConfigFile, WaspProjectDir)

-- | An executable and its arguments, to run from a @dir@.
data NodeCommand dir = NodeCommand String [String]

-- | Runs the command from the dir, like an npm script would.
proc :: Path' Abs (Dir dir) -> NodeCommand dir -> J.CreateJobProcess
proc dir (NodeCommand executable args) = J.setCwd dir $ J.proc executable args

-- | The command as a shell command line, e.g. for an npm script.
toShellCommand :: NodeCommand dir -> String
toShellCommand (NodeCommand executable args) = unwords $ executable : args

-- | Type-checks all of the user's code at once, client and server, so it isn't
-- in any generator's commands.
typeCheckUserCode :: Path' (Rel WaspProjectDir) (File SrcTsConfigFile) -> NodeCommand WaspProjectDir
typeCheckUserCode srcTsConfigPath =
  NodeCommand "tsc" ["--project", SP.fromRelFile srcTsConfigPath, "--noEmit"]
