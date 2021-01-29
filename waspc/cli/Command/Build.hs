module Command.Build
    ( build
    ) where

import           Control.Monad.IO.Class   (liftIO)
import           Control.Monad.Except     (throwError)

import           Command                  (Command, CommandError (..))
import           Command.Common           (findWaspProjectRootDirFromCwd,
                                           waspSaysC, compileIOWithOptions)
import           StrongPath               (Abs, Dir, Path, (</>))
import           CompileOptions           (CompileOptions (..))
import qualified Common
import qualified Lib

build :: Command ()
build = do
    waspProjectDir <- findWaspProjectRootDirFromCwd
    let outDir = waspProjectDir </> Common.dotWaspDirInWaspProjectDir
                 </> Common.buildDirInDotWaspDir

    waspSaysC "Building wasp project..."
    buildResult <- liftIO $ buildIO waspProjectDir outDir
    case buildResult of
        Left compileError -> throwError $ CommandError $ "Build failed: " ++ compileError
        Right () -> waspSaysC "Code has been successfully built!\n"
    

buildIO :: Path Abs (Dir Common.WaspProjectDir)
        -> Path Abs (Dir Lib.ProjectRootDir)
        -> IO (Either String ())
buildIO waspProjectDir outDir = compileIOWithOptions options waspProjectDir outDir
    where
        options = CompileOptions
            { externalCodeDirPath = waspProjectDir </> Common.extCodeDirInWaspProjectDir
            , isBuild = True
            }
