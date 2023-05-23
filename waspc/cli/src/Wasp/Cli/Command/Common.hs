module Wasp.Cli.Command.Common
  ( findWaspProjectRootDirFromCwd,
    findWaspProjectRoot,
    readWaspCompileInfo,
    throwIfExeIsNotAvailable,
    commandRequires,
    CommandRequirement (..),
  )
where

import Control.Monad.Except
import qualified Control.Monad.Except as E
import Data.Maybe (fromJust)
import StrongPath (Abs, Dir, Path')
import qualified StrongPath as SP
import StrongPath.Operations
import System.Directory (doesFileExist, doesPathExist, findExecutable, getCurrentDirectory)
import qualified System.FilePath as FP
import Wasp.Cli.Command (Command, CommandError (..))
import Wasp.Cli.Command.Requires (CommandRequirement (..), checkRequirement)
import Wasp.Cli.Common (dotWaspRootFileInWaspProjectDir)
import qualified Wasp.Cli.Common as Cli.Common
import Wasp.Generator.DbGenerator.Operations (dbIsRunning)
import Wasp.Project (WaspProjectDir)
import Wasp.Util (ifM)
import qualified Wasp.Util.IO as IOUtil

findWaspProjectRoot :: Path' Abs (Dir ()) -> Command (Path' Abs (Dir WaspProjectDir))
findWaspProjectRoot currentDir = do
  let absCurrentDirFp = SP.fromAbsDir currentDir
  doesCurrentDirExist <- liftIO $ doesPathExist absCurrentDirFp
  unless doesCurrentDirExist (throwError notFoundError)
  let dotWaspRootFilePath = absCurrentDirFp FP.</> SP.fromRelFile dotWaspRootFileInWaspProjectDir
  isCurrentDirRoot <- liftIO $ doesFileExist dotWaspRootFilePath
  if isCurrentDirRoot
    then return $ SP.castDir currentDir
    else do
      let parentDir = SP.parent currentDir
      when (parentDir == currentDir) (throwError notFoundError)
      findWaspProjectRoot parentDir
  where
    notFoundError =
      CommandError
        "Wasp command failed"
        ( "Couldn't find wasp project root - make sure"
            ++ " you are running this command from a Wasp project."
        )

findWaspProjectRootDirFromCwd :: Command (Path' Abs (Dir WaspProjectDir))
findWaspProjectRootDirFromCwd = do
  absCurrentDir <- liftIO getCurrentDirectory
  findWaspProjectRoot (fromJust $ SP.parseAbsDir absCurrentDir)

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

-- | @commandRequires requirement@ checks whether the specified requirement is
-- met, throwing an error if it is not.
--
-- See 'CommandRequirement' for what each requirement is and what it checks
-- for.
--
-- NOTE: it would be better if this function was in 'Wasp.Cli.Command.Requires',
-- but that creates a dependency loop (since this function depends on Common, which
-- depends on Command, which depends on Requires).
commandRequires :: CommandRequirement -> Command ()
commandRequires r = case r of
  DbConnection ->
    req
      ( do
          waspRoot <- findWaspProjectRootDirFromCwd
          let outDir = waspRoot </> Cli.Common.dotWaspDirInWaspProjectDir </> Cli.Common.generatedCodeDirInDotWaspDir
          liftIO $ dbIsRunning outDir
      )
      ( CommandError
          "Can not connect to database"
          "The database needs to be running in order to execute this command. Make sure `wasp start db` is running and try again."
      )
  where
    req check err = do
      met <- checkRequirement r check
      if met
        then return ()
        else throwError err
