module Wasp.Cli.Command.Compile
  ( compileIO,
    compile,
    compileIOWithOptions,
  )
where

import Control.Monad.Except (runExceptT, throwError)
import Control.Monad.IO.Class (liftIO)
import StrongPath (Abs, Dir, Path', (</>))
import Wasp
import Wasp.Cli.Command (Command, CommandError (..))
import Wasp.Cli.Command.Common
  ( findWaspProjectRootDirFromCwd,
    waspSaysC,
  )
import Wasp.Cli.Command.Db.Migrate
  ( MigrationDirCopyDirection (..),
    copyDbMigrationsDir,
  )
import qualified Wasp.Cli.Common as Common
import Wasp.Common (WaspProjectDir)
import Wasp.CompileOptions (CompileOptions (..))
import qualified Wasp.Lib

compile :: Command ()
compile = do
  waspProjectDir <- findWaspProjectRootDirFromCwd
  let outDir =
        waspProjectDir </> Common.dotWaspDirInWaspProjectDir
          </> Common.generatedCodeDirInDotWaspDir

  waspSaysC "Compiling wasp code..."
  compilationResult <- liftIO $ compileIO waspProjectDir outDir
  case compilationResult of
    Left compileError -> throwError $ CommandError $ "Compilation failed: " ++ compileError
    Right _ -> waspSaysC "Code has been successfully compiled, project has been generated.\n"

-- | Compiles Wasp source code in waspProjectDir directory and generates a project
--   in given outDir directory.
compileIO ::
  Path' Abs (Dir WaspProjectDir) ->
  Path' Abs (Dir Wasp.Lib.ProjectRootDir) ->
  IO (Either String Wasp)
compileIO waspProjectDir outDir = compileIOWithOptions options waspProjectDir outDir
  where
    options =
      CompileOptions
        { externalCodeDirPath = waspProjectDir </> Common.extCodeDirInWaspProjectDir,
          isBuild = False
        }

compileIOWithOptions ::
  CompileOptions ->
  Path' Abs (Dir Common.WaspProjectDir) ->
  Path' Abs (Dir Wasp.Lib.ProjectRootDir) ->
  IO (Either String Wasp)
compileIOWithOptions options waspProjectDir outDir = runExceptT $ do
  -- TODO: Use throwIO instead of Either to return exceptions?
  wasp <-
    liftIO (Wasp.Lib.compile waspProjectDir outDir options)
      >>= either throwError return
  liftIO (copyDbMigrationsDir CopyMigDirDown waspProjectDir outDir)
    >>= maybe (return ()) (throwError . ("Copying migration folder failed: " ++))
  return wasp
