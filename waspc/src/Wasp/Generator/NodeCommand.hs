module Wasp.Generator.NodeCommand
  ( NodeCommand (..),
    proc,
    toShellCommand,
  )
where

import StrongPath (Abs, Dir, Path')
import qualified Wasp.Job as J

-- | An executable and its arguments, to run from a @dir@.
data NodeCommand dir = NodeCommand String [String]

-- | Runs the command from the dir, like an npm script would.
proc :: Path' Abs (Dir dir) -> NodeCommand dir -> J.CreateJobProcess
proc dir (NodeCommand executable args) = J.setCwd dir $ J.proc executable args

-- | The command as a shell command line, e.g. for an npm script.
toShellCommand :: NodeCommand dir -> String
toShellCommand (NodeCommand executable args) = unwords $ executable : args
