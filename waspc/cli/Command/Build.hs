module Command.Build
  ( build,
  )
where

import qualified Cli.Common as Common
import Command (Command, CommandError (..))
import Command.Common
  ( alphaWarningMessage,
    findWaspProjectRootDirFromCwd,
  )
import Command.Compile (compileIOWithOptions)
import CompileOptions (CompileOptions (..))
import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import qualified Lib
import StrongPath (Abs, Dir, Path, (</>))

build :: Command ()
build = do
  waspProjectDir <- findWaspProjectRootDirFromCwd
  let outDir =
        waspProjectDir </> Common.dotWaspDirInWaspProjectDir
          </> Common.buildDirInDotWaspDir

  liftIO $ putStrLn "Building wasp project..."
  buildResult <- liftIO $ buildIO waspProjectDir outDir
  case buildResult of
    Left compileError -> throwError $ CommandError $ "Build failed: " ++ compileError
    Right () -> liftIO $ putStrLn "Code has been successfully built! Check it out in .wasp/build directory.\n"
  liftIO $ putStrLn alphaWarningMessage

buildIO ::
  Path Abs (Dir Common.WaspProjectDir) ->
  Path Abs (Dir Lib.ProjectRootDir) ->
  IO (Either String ())
buildIO waspProjectDir outDir = compileIOWithOptions options waspProjectDir outDir
  where
    options =
      CompileOptions
        { externalCodeDirPath = waspProjectDir </> Common.extCodeDirInWaspProjectDir,
          isBuild = True
        }
