module Command.Compile
    ( compileIO
    , compile
    , compileIOWithOptions
    ) where

import           Control.Monad.Except   (throwError, runExceptT)
import           Control.Monad.IO.Class (liftIO)

import           Command                (Command, CommandError (..))
import           Command.Common         (findWaspFile,
                                         findWaspProjectRootDirFromCwd,
                                         waspSaysC)
import           Command.Db.Migrate     (copyDbMigrationsDir, MigrationDirCopyDirection(..))

import qualified Common
import           CompileOptions         (CompileOptions (..))
import qualified Lib
import           StrongPath             (Abs, Dir, Path, (</>))


compile :: Command ()
compile = do
    waspProjectDir <- findWaspProjectRootDirFromCwd
    let outDir = waspProjectDir </> Common.dotWaspDirInWaspProjectDir
                 </> Common.generatedCodeDirInDotWaspDir

    waspSaysC "Compiling wasp code..."
    compilationResult <- liftIO $ compileIO waspProjectDir outDir
    case compilationResult of
        Left compileError -> throwError $ CommandError $ "Compilation failed: " ++ compileError
        Right () -> waspSaysC "Code has been successfully compiled, project has been generated.\n"

-- | Compiles Wasp source code in waspProjectDir directory and generates a project
--   in given outDir directory.
compileIO :: Path Abs (Dir Common.WaspProjectDir)
        -> Path Abs (Dir Lib.ProjectRootDir)
        -> IO (Either String ())
compileIO waspProjectDir outDir = compileIOWithOptions options waspProjectDir outDir
  where
    options = CompileOptions
        { externalCodeDirPath = waspProjectDir </> Common.extCodeDirInWaspProjectDir
        , waspignoreFilePath = waspProjectDir </> Common.waspignoreFileInWaspProjectDir
        , isBuild = False
        }

compileIOWithOptions :: CompileOptions
                     -> Path Abs (Dir Common.WaspProjectDir)
                     -> Path Abs (Dir Lib.ProjectRootDir)
                     -> IO (Either String ())
compileIOWithOptions options waspProjectDir outDir = runExceptT $ do
    -- TODO: Use throwIO instead of Either to return exceptions?
    waspFile <- liftIO (findWaspFile waspProjectDir)
        >>= maybe (throwError "No *.wasp file present in the root of Wasp project.") return
    liftIO (Lib.compile waspFile outDir options)
        >>= either throwError return
    liftIO (copyDbMigrationsDir CopyMigDirDown waspProjectDir outDir)
        >>= maybe (return ()) (throwError . ("Copying migration folder failed: " ++))
