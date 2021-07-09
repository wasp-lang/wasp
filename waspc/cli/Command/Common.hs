module Command.Common
  ( findWaspProjectRootDirFromCwdIO,
    findWaspProjectRootDirFromCwdCmd,
    findWaspProjectRootIO,
    findWaspProjectRootCmd,
    waspSaysC,
    alphaWarningMessage,
  )
where

import Cli.Common
  ( dotWaspRootFileInWaspProjectDir,
    waspSays,
  )
import Command (Command, CommandError (..))
import Common (WaspProjectDir)
import Control.Monad (unless, when)
import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import Data.Maybe (fromJust)
import StrongPath (Abs, Dir, Path)
import qualified StrongPath as SP
import System.Directory
  ( doesFileExist,
    doesPathExist,
    getCurrentDirectory,
  )
import qualified System.FilePath as FP

-- Damiano: this is for IO
findWaspProjectRootIO :: Path Abs (Dir ()) -> IO (Maybe (Path Abs (Dir WaspProjectDir)))
findWaspProjectRootIO currentDir = do
  let absCurrentDirFp = SP.toFilePath currentDir
  absCurrentDirFpExists <- doesPathExist absCurrentDirFp
  if (not absCurrentDirFpExists)
    then return Nothing
    else do
      let dotWaspRootFilePath = absCurrentDirFp FP.</> SP.toFilePath dotWaspRootFileInWaspProjectDir
      dotWaspRootFilePathExists <- doesFileExist dotWaspRootFilePath
      if (not dotWaspRootFilePathExists)
        then do
          let parentDir = SP.parent currentDir
          if (parentDir == currentDir)
            then return Nothing
            else findWaspProjectRootIO parentDir
        else return $ Just (SP.castDir currentDir)

-- Damiano: this is for Command
findWaspProjectRootCmd :: Path Abs (Dir ()) -> Command (Path Abs (Dir WaspProjectDir))
findWaspProjectRootCmd currentDir = do
  maybeWaspProjectRoot <- liftIO (findWaspProjectRootIO currentDir)
  case maybeWaspProjectRoot of
    Nothing -> error "Project root directory not found!"
    Just waspProjectRoot -> return waspProjectRoot

-- Damiano: IO
findWaspProjectRootDirFromCwdIO :: IO (Path Abs (Dir WaspProjectDir))
findWaspProjectRootDirFromCwdIO = do
  absCurrentDir <- liftIO getCurrentDirectory
  maybeWaspProjectRoot <- findWaspProjectRootIO (fromJust $ SP.parseAbsDir absCurrentDir)
  case maybeWaspProjectRoot of
    Nothing -> error "Project root directory not found!"
    Just waspProjectRoot -> return waspProjectRoot

-- Damiano: Command
findWaspProjectRootDirFromCwdCmd :: Command (Path Abs (Dir WaspProjectDir))
findWaspProjectRootDirFromCwdCmd = do
  absCurrentDir <- liftIO getCurrentDirectory
  findWaspProjectRootCmd (fromJust $ SP.parseAbsDir absCurrentDir)

waspSaysC :: String -> Command ()
waspSaysC = liftIO . waspSays

alphaWarningMessage :: String
alphaWarningMessage =
  ( "NOTE: Wasp is still in Alpha, therefore not yet production ready "
      ++ "and might change significantly in the future versions."
  )
