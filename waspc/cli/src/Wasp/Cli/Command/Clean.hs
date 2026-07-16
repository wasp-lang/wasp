module Wasp.Cli.Command.Clean
  ( clean,
  )
where

import qualified Options.Applicative as Opt
import qualified StrongPath as SP
import Wasp.Cli.Command (Command)
import Wasp.Cli.Command.Call (Arguments)
import Wasp.Cli.Command.Common (deleteDirectoryContentsVerboselyExcept, deleteDirectoryIfExistsVerbosely)
import Wasp.Cli.Command.Message (cliSendMessageC)
import Wasp.Cli.Command.Require (InWaspProject (InWaspProject), require)
import Wasp.Cli.Util.Parser (withArguments)
import qualified Wasp.Message as Msg
import Wasp.Project.Common (dotWaspDirInWaspProjectDir, nodeModulesDirInWaspProjectDir, stateDirInDotWaspDir)
import Wasp.Util.Terminal (styleCode)

clean :: Arguments -> Command ()
clean = withArguments "wasp clean" cleanArgsParser $ \cleanArgs -> do
  InWaspProject waspProjectDir <- require

  let dotWaspDir = waspProjectDir SP.</> dotWaspDirInWaspProjectDir
  let nodeModulesDir = waspProjectDir SP.</> nodeModulesDirInWaspProjectDir

  let dotWaspContentsToKeep =
        if deleteData cleanArgs
          then ([], [])
          else ([], [stateDirInDotWaspDir])

  deleteDirectoryContentsVerboselyExcept dotWaspDir dotWaspContentsToKeep

  deleteDirectoryIfExistsVerbosely nodeModulesDir

  cliSendMessageC $
    Msg.Info $
      "\nRun " ++ styleCode "wasp install" ++ " to reinstall dependencies."

data CleanArgs = CleanArgs
  { deleteData :: Bool
  }

cleanArgsParser :: Opt.Parser CleanArgs
cleanArgsParser =
  CleanArgs
    <$> Opt.switch
      ( Opt.long "data"
          <> Opt.help "Also delete persistent Wasp state, including the managed SQLite database"
      )
