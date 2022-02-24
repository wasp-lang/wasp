module Wasp.Cli.Command.Build
  ( build,
  )
where

import Control.Monad (when)
import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import StrongPath (Abs, Dir, Path', (</>))
import qualified StrongPath as SP
import System.Directory
  ( doesDirectoryExist,
    removeDirectoryRecursive,
  )
import Wasp.Cli.Command (Command, CommandError (..))
import Wasp.Cli.Command.Common
  ( alphaWarningMessage,
    findWaspProjectRootDirFromCwd,
    waspSaysC,
  )
import Wasp.Cli.Command.Compile (compileIOWithOptions)
import qualified Wasp.Cli.Common as Common
import Wasp.Cli.Message (cliSendMessage)
import Wasp.Cli.Terminal (asWaspFailureMessage, asWaspStartMessage, asWaspSuccessMessage)
import Wasp.CompileOptions (CompileOptions (..))
import qualified Wasp.Lib

build :: Command ()
build = do
  waspProjectDir <- findWaspProjectRootDirFromCwd
  let buildDir =
        waspProjectDir </> Common.dotWaspDirInWaspProjectDir
          </> Common.buildDirInDotWaspDir
      buildDirFilePath = SP.fromAbsDir buildDir

  doesBuildDirExist <- liftIO $ doesDirectoryExist buildDirFilePath
  when doesBuildDirExist $ do
    waspSaysC $ asWaspStartMessage "Clearing the content of the .wasp/build directory..."
    liftIO $ removeDirectoryRecursive buildDirFilePath
    waspSaysC $ asWaspSuccessMessage "Successfully cleared the contents of the .wasp/build directory."

  waspSaysC $ asWaspStartMessage "Building wasp project..."
  buildResult <- liftIO $ buildIO waspProjectDir buildDir
  case buildResult of
    Left compileError -> throwError $ CommandError $ asWaspFailureMessage "Build failed:" ++ compileError
    Right () -> waspSaysC $ asWaspSuccessMessage "Code has been successfully built! Check it out in .wasp/build directory."
  waspSaysC alphaWarningMessage

buildIO ::
  Path' Abs (Dir Common.WaspProjectDir) ->
  Path' Abs (Dir Wasp.Lib.ProjectRootDir) ->
  IO (Either String ())
buildIO waspProjectDir buildDir = compileIOWithOptions options waspProjectDir buildDir
  where
    options =
      CompileOptions
        { externalCodeDirPath = waspProjectDir </> Common.extCodeDirInWaspProjectDir,
          isBuild = True,
          sendMessage = cliSendMessage
        }
