module Command.Common
    ( findWaspProjectRootDirFromCwd
    , findWaspProjectRoot
    , waspSaysC
    ) where

import System.Directory (getCurrentDirectory, doesPathExist, doesFileExist)
import qualified System.FilePath as FP
import Data.Maybe (fromJust)
import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import Control.Monad (when, unless)

import StrongPath (Path, Abs, Dir)
import qualified StrongPath as SP
import Command (Command, CommandError(..))
import Common (WaspProjectDir, dotWaspRootFileInWaspProjectDir, waspSays)


findWaspProjectRoot :: Path Abs (Dir ()) -> Command (Path Abs (Dir WaspProjectDir))
findWaspProjectRoot currentDir = do
    let absCurrentDirFp = SP.toFilePath currentDir
    doesCurrentDirExist <- liftIO $ doesPathExist absCurrentDirFp
    unless doesCurrentDirExist (throwError notFoundError)
    let dotWaspRootFilePath = absCurrentDirFp FP.</> SP.toFilePath dotWaspRootFileInWaspProjectDir
    isCurrentDirRoot <- liftIO $ doesFileExist dotWaspRootFilePath
    if isCurrentDirRoot
        then return $ SP.castDir currentDir
        else do let parentDir = SP.parent currentDir
                when (parentDir == currentDir) (throwError notFoundError)
                findWaspProjectRoot parentDir
  where
      notFoundError = CommandError ("Couldn't find wasp project root - make sure"
                                    ++ " you are running this command from Wasp project.")

findWaspProjectRootDirFromCwd :: Command (Path Abs (Dir WaspProjectDir))
findWaspProjectRootDirFromCwd = do
    absCurrentDir <- liftIO getCurrentDirectory
    findWaspProjectRoot (fromJust $ SP.parseAbsDir absCurrentDir)

waspSaysC :: String -> Command ()
waspSaysC = liftIO . waspSays
