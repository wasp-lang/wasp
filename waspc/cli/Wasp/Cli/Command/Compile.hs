module Wasp.Cli.Command.Compile
  ( compileIO,
    compile,
    compileIOWithOptions,
  )
where

import Control.Monad.Except (runExceptT, throwError)
import Control.Monad.IO.Class (liftIO)
import StrongPath (Abs, Dir, Path', (</>))
import Wasp.Cli.Command (Command, CommandError (..))
import Wasp.Cli.Command.Common
  ( findWaspProjectRootDirFromCwd,
    waspSaysC,
  )
import qualified Wasp.Cli.Common as Common
import Wasp.Cli.Terminal (asWaspFailureMessage, asWaspStartMessage, asWaspSuccessMessage)
import Wasp.Common (WaspProjectDir)
import Wasp.CompileOptions (CompileOptions (..))
import qualified Wasp.Lib

compile :: Command ()
compile = do
  waspProjectDir <- findWaspProjectRootDirFromCwd
  let outDir =
        waspProjectDir </> Common.dotWaspDirInWaspProjectDir
          </> Common.generatedCodeDirInDotWaspDir

  waspSaysC $ asWaspStartMessage "Compiling wasp code..."
  compilationResult <- liftIO $ compileIO waspProjectDir outDir
  case compilationResult of
    Left compileError -> throwError $ CommandError $ asWaspFailureMessage "Compilation failed:" ++ compileError
    Right () -> waspSaysC $ asWaspSuccessMessage "Code has been successfully compiled, project has been generated."

-- | Compiles Wasp source code in waspProjectDir directory and generates a project
--   in given outDir directory.
compileIO ::
  Path' Abs (Dir WaspProjectDir) ->
  Path' Abs (Dir Wasp.Lib.ProjectRootDir) ->
  IO (Either String ())
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
  IO (Either String ())
compileIOWithOptions options waspProjectDir outDir = runExceptT $ do
  -- TODO: Use throwIO instead of Either to return exceptions?
  liftIO (Wasp.Lib.compile waspProjectDir outDir options)
    >>= either throwError return
