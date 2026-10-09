{-# LANGUAGE GADTs #-}

module Wasp.Job.CreateProcess
  ( proc,
    CreateJobProcess (..),
    setCwd,
    asCreateProcess,
    markInteractive,
  )
where

import StrongPath (Abs, Dir, Path')
import qualified StrongPath as SP
import System.Environment (getEnvironment)
import qualified System.Process as P
import Wasp.Env (EnvVar, HasEnvVars (..), addEnvVarsOverride)

data CreateJobProcess
  = CreateJobProcess
  { bin :: String,
    args :: [String],
    cwd :: Maybe (Path' Abs (Dir ())),
    inheritEnv :: Bool,
    envVars :: [EnvVar],
    interactive :: Bool
  }

instance HasEnvVars CreateJobProcess where
  getEnvVars = envVars
  setEnvVars newEnv cjp = cjp {envVars = newEnv}

proc :: String -> [String] -> CreateJobProcess
proc bin args =
  CreateJobProcess
    { bin = bin,
      args = args,
      cwd = Nothing,
      inheritEnv = True,
      envVars = [],
      interactive = False
    }

setCwd :: Path' Abs (Dir a) -> CreateJobProcess -> CreateJobProcess
setCwd newCwd cjp = cjp {cwd = Just $ SP.castDir newCwd}

markInteractive :: CreateJobProcess -> CreateJobProcess
markInteractive cjp = cjp {interactive = True}

asCreateProcess :: CreateJobProcess -> IO P.CreateProcess
asCreateProcess cjp = do
  inheritedEnv <- if cjp.inheritEnv then getEnvironment else pure []
  let fullEnv = inheritedEnv `addEnvVarsOverride` cjp.envVars
  pure $
    (P.proc cjp.bin cjp.args)
      { P.cwd = SP.fromAbsDir <$> cwd cjp,
        P.env = Just fullEnv,
        P.std_in =
          if cjp.interactive
            then P.Inherit
            else P.CreatePipe -- Non-interactive jobs just get an empty pipe for stdin
      }
