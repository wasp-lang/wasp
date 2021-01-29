module Command.Common
    ( findWaspProjectRootDirFromCwd
    , findWaspProjectRoot
    , findWaspFile
    , compileIOWithOptions
    , waspSaysC
    ) where

import           Control.Monad          (unless, when)
import           Control.Monad.Except   (throwError)
import           Control.Monad.IO.Class (liftIO)
import           Data.Maybe             (fromJust)
import           Data.List              (find, isSuffixOf)
import           System.Directory       (doesFileExist, doesPathExist,
                                         getCurrentDirectory)
import qualified Path                   as P
import qualified System.FilePath        as FP

import           Command                (Command, CommandError (..))
import           Common                 (WaspProjectDir,
                                         dotWaspRootFileInWaspProjectDir,
                                         waspSays)
import qualified Lib
import           CompileOptions         (CompileOptions (..))
import           StrongPath             (Abs, Dir, Path)
import qualified StrongPath             as SP
import qualified Util.IO

compileIOWithOptions :: CompileOptions
                    -> Path Abs (Dir WaspProjectDir)
                    -> Path Abs (Dir Lib.ProjectRootDir)
                    -> IO (Either String ())
compileIOWithOptions options waspProjectDir outDir = do
    maybeWaspFile <- findWaspFile waspProjectDir
    case maybeWaspFile of
        Nothing -> return $ Left "No *.wasp file present in the root of Wasp project."
        Just waspFile -> Lib.compile waspFile outDir options
    
findWaspFile :: Path Abs (Dir d) -> IO (Maybe (Path Abs SP.File))
findWaspFile dir = do
    (files, _) <- liftIO $ Util.IO.listDirectory (SP.toPathAbsDir dir)
    return $ (dir SP.</>) . SP.fromPathRelFile <$> find isWaspFile files

isWaspFile :: P.Path P.Rel P.File -> Bool
isWaspFile path = ".wasp" `isSuffixOf` P.toFilePath path
                  && (length (P.toFilePath path) > length (".wasp" :: String))

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
