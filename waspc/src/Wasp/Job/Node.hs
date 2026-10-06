module Wasp.Job.Node (command) where

import Control.Monad.IO.Class (liftIO)
import qualified Data.Text as T
import StrongPath (Abs, Dir, Path')
import qualified StrongPath as SP
import System.Environment (getEnvironment)
import qualified System.Process as P
import qualified Wasp.Job as Job
import qualified Wasp.Node.Version as NodeVersion

-- | Builds a command that uses the user's Node installation, to run with
-- 'Wasp.Job.Process.run'. Fails the job if Node or npm don't meet Wasp's
-- requirements.
command :: [(String, String)] -> Path' Abs (Dir a) -> String -> [String] -> Job.Job P.CreateProcess
command extraEnvVars workingDir executable arguments =
  liftIO NodeVersion.checkUserNodeAndNpmMeetWaspRequirements >>= \case
    NodeVersion.VersionCheckFail message -> do
      Job.emitJobOutput Job.Stderr $ T.pack message
      Job.failWithExitCode 1
    NodeVersion.VersionCheckSuccess -> do
      -- Haskell will use the first value for variable name it finds. Since env
      -- vars in 'extraEnvVars' should override the inherited env vars, we
      -- must prepend them.
      envVars <- (extraEnvVars ++) <$> liftIO getEnvironment
      return $
        (P.proc executable arguments)
          { P.env = Just envVars,
            P.cwd = Just $ SP.fromAbsDir workingDir
          }
