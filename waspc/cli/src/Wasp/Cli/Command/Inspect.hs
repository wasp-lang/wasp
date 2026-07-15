module Wasp.Cli.Command.Inspect
  ( inspect,
  )
where

import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Lazy as BSL
import System.IO (stdout)
import Wasp.Cli.Command (Command)
import Wasp.Cli.Command.Call (Arguments)
import Wasp.Cli.Command.Compile (analyzeWithWarningsOnStderr)
import Wasp.Cli.Command.Inspect.ArgumentsParser (InspectArgs (..), inspectArgsParser)
import Wasp.Cli.Command.Inspect.JSON (inspectAsJson)
import Wasp.Cli.Command.Inspect.Table (inspectAsTables)
import Wasp.Cli.Command.Require (InWaspProject (InWaspProject), ValidNodeAndNpm (ValidNodeAndNpm), WaspSpecAvailable (WaspSpecAvailable), require)
import Wasp.Cli.Util.Parser (withArguments)

-- | Prints the evaluated app spec: as a human-readable overview by default, or
-- as full JSON with --json.
inspect :: Arguments -> Command ()
inspect = withArguments "wasp inspect" inspectArgsParser $ \args -> do
  ValidNodeAndNpm <- require
  InWaspProject waspDir <- require
  WaspSpecAvailable <- require
  appSpec <- analyzeWithWarningsOnStderr waspDir
  liftIO $
    if json args
      then do
        BSL.hPutStr stdout $ inspectAsJson appSpec
        putStrLn ""
      else
        putStr $ inspectAsTables appSpec
