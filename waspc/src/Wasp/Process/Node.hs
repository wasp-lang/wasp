module Wasp.Process.Node (prepare) where

import StrongPath (Abs, Dir, Path')
import qualified StrongPath as SP
import System.Environment (getEnvironment)
import qualified System.Process as P
import qualified Wasp.Node.Version as NodeVersion

prepare :: [(String, String)] -> Path' Abs (Dir a) -> String -> [String] -> IO (Either String P.CreateProcess)
prepare extraEnvVars workingDir command args =
  NodeVersion.checkUserNodeAndNpmMeetWaspRequirements >>= \case
    NodeVersion.VersionCheckFail message -> return $ Left message
    NodeVersion.VersionCheckSuccess -> Right <$> makeCreateProcess extraEnvVars workingDir command args

makeCreateProcess :: [(String, String)] -> Path' Abs (Dir a) -> String -> [String] -> IO P.CreateProcess
makeCreateProcess extraEnvVars workingDir executable arguments = do
  envVars <- getAllEnvVars
  return $ (P.proc executable arguments) {P.env = Just envVars, P.cwd = Just $ SP.fromAbsDir workingDir}
  where
    -- Haskell will use the first value for variable name it finds. Since env
    -- vars in 'extraEnvVars' should override the inherited env vars, we
    -- must prepend them.
    getAllEnvVars = (extraEnvVars ++) <$> getEnvironment
