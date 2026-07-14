module Wasp.Cli.Command.Clean
  ( clean,
  )
where

import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import qualified Options.Applicative as Opt
import qualified StrongPath as SP
import Wasp.Cli.Command (Command)
import Wasp.Cli.Command.Call (Arguments)
import Wasp.Cli.Command.Common (deleteDirectoryIfExistsVerbosely)
import Wasp.Cli.Command.Message (cliSendMessageC)
import Wasp.Cli.Command.Require (InWaspProject (InWaspProject), require)
import Wasp.Cli.Util.Parser (withArguments)
import qualified Wasp.Message as Msg
import Wasp.Project.Common (dotWaspDirInWaspProjectDir, generatedAppDirInWaspProjectDir, nodeModulesDirInWaspProjectDir)
import qualified Wasp.Util.IO as IOUtil
import Wasp.Util.Terminal (styleCode)

clean :: Arguments -> Command ()
clean = withArguments "wasp clean" cleanArgsParser $ \cleanArgs -> do
  InWaspProject waspProjectDir <- require

  let dotWaspDir = waspProjectDir SP.</> dotWaspDirInWaspProjectDir
  let generatedAppDir = waspProjectDir SP.</> generatedAppDirInWaspProjectDir
  let nodeModulesDir = waspProjectDir SP.</> nodeModulesDirInWaspProjectDir

  if deleteData cleanArgs
    then deleteDirectoryIfExistsVerbosely dotWaspDir
    else do
      deleteDirectoryIfExistsVerbosely generatedAppDir
      liftIO $ IOUtil.deleteDirectoryIfExists $ dotWaspDir SP.</> [SP.reldir|spec|]
      liftIO $ IOUtil.deleteFileIfExists $ dotWaspDir SP.</> [SP.relfile|spec-result.json|]
      dotWaspDirExists <- liftIO $ IOUtil.doesDirectoryExist dotWaspDir
      when dotWaspDirExists $ do
        dotWaspDirIsEmpty <- liftIO $ IOUtil.isDirectoryEmpty dotWaspDir
        when dotWaspDirIsEmpty $ liftIO $ IOUtil.removeDirectory dotWaspDir
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
