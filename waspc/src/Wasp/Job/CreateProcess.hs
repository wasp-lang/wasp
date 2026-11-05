module Wasp.Job.CreateProcess
  ( proc,
    CreateJobProcess,
    setCwd,
    asCreateProcess,
    markInteractive,
    addEnvVarsOverride,
  )
where

import StrongPath (Abs, Dir, Path')
import qualified StrongPath as SP
import System.Directory (getCurrentDirectory)
import System.Environment (getEnvironment)
import qualified System.Process as P
import Wasp.Env (EnvVar)
import qualified Wasp.Env as Env
import Wasp.Job.CreateProcess.SearchPath (findExecutableFrom, getSearchPathEnvVars)

data CreateJobProcess
  = CreateJobProcess
  { bin :: String,
    args :: [String],
    cwd :: Maybe (Path' Abs (Dir ())),
    envVars :: [EnvVar],
    interactive :: Bool
  }

-- | A process that runs the given executable, an npm package's one or one in
-- the @PATH@ (see 'asCreateProcess'), with the given arguments.
proc :: String -> [String] -> CreateJobProcess
proc bin args =
  CreateJobProcess
    { bin = bin,
      args = args,
      cwd = Nothing,
      envVars = [],
      interactive = False
    }

setCwd :: Path' Abs (Dir a) -> CreateJobProcess -> CreateJobProcess
setCwd newCwd cjp = cjp {cwd = Just $ SP.castDir newCwd}

markInteractive :: CreateJobProcess -> CreateJobProcess
markInteractive cjp = cjp {interactive = True}

-- | Adds env vars to the process. If there are duplicates, the incoming env
-- vars override the existing ones.
addEnvVarsOverride :: [EnvVar] -> CreateJobProcess -> CreateJobProcess
addEnvVarsOverride incoming cjp = cjp {envVars = Env.addEnvVarsOverride incoming cjp.envVars}

-- | The process runs like an npm script would in its working directory. Fails if there is no such executable.
asCreateProcess :: CreateJobProcess -> IO P.CreateProcess
asCreateProcess cjp = do
  processCwd <- maybe (SP.parseAbsDir =<< getCurrentDirectory) return cjp.cwd
  bin <- findExecutableFrom processCwd cjp.bin
  searchPathEnvVars <- getSearchPathEnvVars processCwd
  inheritedEnv <- getEnvironment
  let fullEnv = cjp.envVars `Env.addEnvVarsOverride` (searchPathEnvVars `Env.addEnvVarsOverride` inheritedEnv)
  pure $
    (P.proc bin cjp.args)
      { P.cwd = SP.fromAbsDir <$> cwd cjp,
        P.env = Just fullEnv,
        P.std_in =
          if cjp.interactive
            then P.Inherit
            else P.CreatePipe -- Non-interactive jobs just get an empty pipe for stdin
      }
