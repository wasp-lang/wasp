module Wasp.Cli.Command.Build.Client
  ( buildClient,
  )
where

import Data.Function ((&))
import StrongPath (Abs, Dir, Path')
import Wasp.Env (EnvVar)
import qualified Wasp.Job as J
import Wasp.Job.Except (ExceptJob, toExceptJob)
import Wasp.Job.Process (runNodeCommandAsJobWithExtraEnv)
import Wasp.Project.Common (WaspProjectDir)

-- | Builds the client with Vite into the generated web-app dir. The client reads its
-- `REACT_APP_*` env vars at build time, from the given env vars and the current environment.
buildClient :: [EnvVar] -> Path' Abs (Dir WaspProjectDir) -> ExceptJob
buildClient envVars waspProjectDir =
  runNodeCommandAsJobWithExtraEnv envVars waspProjectDir "npx" ["vite", "build"] J.WebApp
    & toExceptJob (("Building the client failed with exit code: " <>) . show)
