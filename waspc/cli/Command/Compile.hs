module Command.Compile
  ( compileIO,
    compile,
    compileIOWithOptions,
  )
where

import qualified Cli.Common
import Command (Command, CommandError (..))
import Command.Common
  ( findWaspProjectRootDirFromCwdCmd,
    waspSaysC,
  )
import Command.Db.Migrate
  ( MigrationDirCopyDirection (..),
    copyDbMigrationsDir,
  )
import Common (WaspProjectDir)
import CompileOptions (CompileOptions (..))
import Control.Monad.Except (runExceptT, throwError)
import Control.Monad.IO.Class (liftIO)
import qualified Lib
import StrongPath (Abs, Dir, Path, (</>))

compile :: Command ()
compile = do
  waspProjectDir <- findWaspProjectRootDirFromCwdCmd
  let outDir =
        waspProjectDir </> Cli.Common.dotWaspDirInWaspProjectDir
          </> Cli.Common.generatedCodeDirInDotWaspDir

  waspSaysC "Compiling wasp code..."
  compilationResult <- liftIO $ compileIO waspProjectDir outDir
  case compilationResult of
    Left compileError -> throwError $ CommandError $ "Compilation failed: " ++ compileError
    Right () -> waspSaysC "Code has been successfully compiled, project has been generated.\n"

-- | Compiles Wasp source code in waspProjectDir directory and generates a project
--   in given outDir directory.
compileIO ::
  Path Abs (Dir WaspProjectDir) ->
  Path Abs (Dir Lib.ProjectRootDir) ->
  IO (Either String ())
compileIO waspProjectDir outDir = compileIOWithOptions options waspProjectDir outDir
  where
    options =
      CompileOptions
        { externalCodeDirPath = waspProjectDir </> Cli.Common.extCodeDirInWaspProjectDir,
          isBuild = False
        }

compileIOWithOptions ::
  CompileOptions ->
  Path Abs (Dir Cli.Common.WaspProjectDir) ->
  Path Abs (Dir Lib.ProjectRootDir) ->
  IO (Either String ())
compileIOWithOptions options waspProjectDir outDir = runExceptT $ do
  -- TODO: Use throwIO instead of Either to return exceptions?
  liftIO (Lib.compile waspProjectDir outDir options)
    >>= either throwError return
  liftIO (copyDbMigrationsDir CopyMigDirDown waspProjectDir outDir)
    >>= maybe (return ()) (throwError . ("Copying migration folder failed: " ++))
