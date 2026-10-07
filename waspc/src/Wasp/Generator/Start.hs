module Wasp.Generator.Start
  ( start,
  )
where

import StrongPath (Abs, Dir, Path')
import Wasp.Generator.Common (GeneratedAppDir)
import Wasp.Generator.ServerGenerator.RunConfig (ServerRunConfig)
import Wasp.Generator.ServerGenerator.Start (startServer)
import Wasp.Generator.WebAppGenerator.RunConfig (WebAppRunConfig)
import Wasp.Generator.WebAppGenerator.Start (startWebApp)
import qualified Wasp.Job.Fictional as J
import Wasp.Project.Common (WaspProjectDir)

-- | This is a blocking action, that will start the processes that run web app and server.
--   It will run as long as one of those processes does not fail.
--   It alo receives 'onJobsQuietDown' IO action, which it executes every time all the processes
--   go quiet (don't produce any stdout/err) for some time (5s), after they have previously
--   produced some output.
start :: (WebAppRunConfig, ServerRunConfig) -> Path' Abs (Dir WaspProjectDir) -> Path' Abs (Dir GeneratedAppDir) -> IO () -> IO (Either String ())
start (webAppRunConfig, serverRunConfig) waspProjectDir outDir onJobsQuietDown = do
  serverOrWebExitCode <-
    J.run
      $ withJobsQuietDownListener onJobsQuietDown
      $ J.race
        (J.prefixWith J.Server $ startServer serverRunConfig outDir)
        (J.prefixWith J.WebApp $ startWebApp webAppRunConfig waspProjectDir)

  case serverOrWebExitCode of
    Left serverExitCode -> return $ Left $ "Server failed with exit code " ++ show serverExitCode ++ "."
    Right webAppExitCode -> return $ Left $ "Web app failed with exit code " ++ show webAppExitCode ++ "."

-- | Gives the action a function to call on every job output. Stops listening
-- once the action returns.
withJobsQuietDownListener :: IO () -> J.Job e a -> J.Job e a
withJobsQuietDownListener _ _ =
  undefined
