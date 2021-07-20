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

findWaspProjectRootIO :: Path Abs (Dir ()) -> IO (Maybe (Path Abs (Dir WaspProjectDir)))
findWaspProjectRootIO currentDir = do
  let absCurrentDirFp = SP.toFilePath currentDir
  doesCurrentDirExist <- doesPathExist absCurrentDirFp
  if (not doesCurrentDirExist)
    then return Nothing
    else do
      let dotWaspRootFilePath = absCurrentDirFp FP.</> SP.toFilePath dotWaspRootFileInWaspProjectDir
      dotWaspRootFileExists <- doesFileExist dotWaspRootFilePath
      if dotWaspRootFileExists
        then return $ Just (SP.castDir currentDir)
        else do
          let parentDir = SP.parent currentDir
          if (parentDir == currentDir)
            then return Nothing
            else findWaspProjectRootIO parentDir

findWaspProjectRootCmd :: Path Abs (Dir ()) -> Command (Path Abs (Dir WaspProjectDir))
findWaspProjectRootCmd currentDir = do
  maybeWaspProjectRoot <- liftIO (findWaspProjectRootIO currentDir)
  case maybeWaspProjectRoot of
    Nothing -> throwError notFoundError
    Just waspProjectRoot -> return waspProjectRoot

findWaspProjectRootDirFromCwdIO :: IO (Maybe (Path Abs (Dir WaspProjectDir)))
findWaspProjectRootDirFromCwdIO = do
  absCurrentDir <- getCurrentDirectory
  maybeWaspProjectRoot <- findWaspProjectRootIO (fromJust $ SP.parseAbsDir absCurrentDir)
  case maybeWaspProjectRoot of
    Nothing -> return Nothing
    Just waspProjectRoot -> return $ Just (SP.castDir waspProjectRoot)

findWaspProjectRootDirFromCwdCmd :: Command (Path Abs (Dir WaspProjectDir))
findWaspProjectRootDirFromCwdCmd = do
  maybeWaspProjectRoot <- liftIO (findWaspProjectRootDirFromCwdIO)
  case maybeWaspProjectRoot of
    Nothing -> throwError notFoundError
    Just waspProjectRoot -> return waspProjectRoot

waspSaysC :: String -> Command ()
waspSaysC = liftIO . waspSays

alphaWarningMessage :: String
alphaWarningMessage =
  ( "NOTE: Wasp is still in Alpha, therefore not yet production ready "
      ++ "and might change significantly in the future versions."
  )

notFoundError :: CommandError
notFoundError =
  CommandError
    ( "Couldn't find wasp project root - make sure"
        ++ " you are running this command from Wasp project."
    )
