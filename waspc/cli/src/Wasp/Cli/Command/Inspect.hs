module Wasp.Cli.Command.Inspect
  ( inspect,
  )
where

import Control.Monad.IO.Class (liftIO)
import Wasp.Cli.Command (Command, require)
import Wasp.Cli.Command.Call (Arguments)
import Wasp.Cli.Command.Compile (analyzeWithDiagnosticsOnStderr)
import Wasp.Cli.Command.Inspect.ArgumentsParser (InspectArgs (..), inspectArgsParser)
import Wasp.Cli.Command.Inspect.JSON (inspectAsJson)
import Wasp.Cli.Command.Inspect.Table (inspectAsTables)
import Wasp.Cli.Command.Require.InWaspProject (InWaspProject (InWaspProject))
import Wasp.Cli.Command.Require.ValidNodeAndNpm (ValidNodeAndNpm (ValidNodeAndNpm))
import Wasp.Cli.Command.Require.WaspSpecAvailable (WaspSpecAvailable (WaspSpecAvailable))
import Wasp.Cli.Util.Parser (withArguments)

-- | Prints the evaluated app spec: as a human-readable overview by default, or
-- as full JSON with --json.
inspect :: Arguments -> Command ()
inspect = withArguments "wasp inspect" inspectArgsParser $ \args -> do
  ValidNodeAndNpm <- require
  InWaspProject waspDir <- require
  WaspSpecAvailable <- require
  appSpec <- analyzeWithDiagnosticsOnStderr waspDir
  liftIO $ putStr $ inspectFn args.json appSpec
  where
    inspectFn isJson
      | isJson = inspectAsJson
      | otherwise = inspectAsTables
