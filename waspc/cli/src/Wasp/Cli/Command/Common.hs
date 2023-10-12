module Wasp.Cli.Command.Common
  ( readWaspCompileInfo,
    throwIfExeIsNotAvailable,
  )
where

import Control.Monad.Except
import qualified Control.Monad.Except as E
import StrongPath (Abs, Dir, Path')
import StrongPath.Operations
import System.Directory (findExecutable)
import Wasp.Cli.Command (Command, CommandError (..))
import qualified Wasp.Cli.Common as Cli.Common
import Wasp.Project (WaspProjectDir)
import Wasp.Util (ifM)
import qualified Wasp.Util.IO as IOUtil

readWaspCompileInfo :: Path' Abs (Dir WaspProjectDir) -> IO String
readWaspCompileInfo waspDir =
  ifM
    (IOUtil.doesFileExist dotWaspInfoFile)
    (IOUtil.readFile dotWaspInfoFile)
    (return "No compile information found")
  where
    dotWaspInfoFile =
      waspDir </> Cli.Common.dotWaspDirInWaspProjectDir
        </> Cli.Common.generatedCodeDirInDotWaspDir
        </> Cli.Common.dotWaspInfoFileInGeneratedCodeDir

throwIfExeIsNotAvailable :: String -> String -> Command ()
throwIfExeIsNotAvailable exeName explanationMsg = do
  liftIO (findExecutable exeName) >>= \case
    Just _ -> return ()
    Nothing ->
      E.throwError $
        CommandError ("Couldn't find `" <> exeName <> "` executable") explanationMsg
