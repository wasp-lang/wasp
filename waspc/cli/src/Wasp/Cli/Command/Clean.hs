module Wasp.Cli.Command.Clean
  ( clean,
    parserInfo,
  )
where

import qualified Options.Applicative as Opt
import qualified StrongPath as SP
import Wasp.Cli.Command (Command, runCommand)
import qualified Wasp.Cli.Command.Call as Call
import Wasp.Cli.Command.Common (deleteDirectoryIfExistsVerbosely)
import Wasp.Cli.Command.Message (cliSendMessageC)
import Wasp.Cli.Command.Require (InWaspProject (InWaspProject), require)
import Wasp.Cli.Command.Telemetry (runWithTelemetry)
import qualified Wasp.Message as Msg
import Wasp.Project.Common (dotWaspDirInWaspProjectDir, nodeModulesDirInWaspProjectDir)
import Wasp.Util.Terminal (styleCode)

parserInfo :: Opt.ParserInfo (IO ())
parserInfo =
  Opt.info
    (pure $ runWithTelemetry Call.Other (runCommand clean))
    (Opt.progDesc "Delete the generated app, cached artifacts, and node_modules. Wasp's \"try turning it off and on again\".")

clean :: Command ()
clean = do
  InWaspProject waspProjectDir <- require

  let dotWaspDir = waspProjectDir SP.</> dotWaspDirInWaspProjectDir
  let nodeModulesDir = waspProjectDir SP.</> nodeModulesDirInWaspProjectDir

  deleteDirectoryIfExistsVerbosely dotWaspDir
  deleteDirectoryIfExistsVerbosely nodeModulesDir

  cliSendMessageC $
    Msg.Info $
      "\nRun " ++ styleCode "wasp install" ++ " to reinstall dependencies."
