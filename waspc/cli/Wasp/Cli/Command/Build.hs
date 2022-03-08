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
  )
import Wasp.Cli.Command.Compile (compileIOWithOptions)
import Wasp.Cli.Command.Message (cliSendMessageC)
import qualified Wasp.Cli.Common as Common
import Wasp.Cli.Message (cliSendMessage)
import Wasp.CompileOptions (CompileOptions (..))
import qualified Wasp.Lib
import qualified Wasp.Message as Msg

build :: Command ()
build = do
  waspProjectDir <- findWaspProjectRootDirFromCwd
  let buildDir =
        waspProjectDir </> Common.dotWaspDirInWaspProjectDir
          </> Common.buildDirInDotWaspDir
      buildDirFilePath = SP.fromAbsDir buildDir

  doesBuildDirExist <- liftIO $ doesDirectoryExist buildDirFilePath
  when doesBuildDirExist $ do
    cliSendMessageC $ Msg.Start "Clearing the content of the .wasp/build directory..."
    liftIO $ removeDirectoryRecursive buildDirFilePath
    cliSendMessageC $ Msg.Success "Successfully cleared the contents of the .wasp/build directory."

  cliSendMessageC $ Msg.Start "Building wasp project..."
  buildResult <- liftIO $ buildIO waspProjectDir buildDir
  case buildResult of
    Left compileError -> throwError $ CommandError "Build failed" compileError
    Right () -> cliSendMessageC $ Msg.Success "Code has been successfully built! Check it out in .wasp/build directory."
  cliSendMessageC $ Msg.Warning "Build warning" alphaWarningMessage

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
