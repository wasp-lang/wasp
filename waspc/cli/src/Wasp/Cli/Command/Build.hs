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
import Wasp.Cli.Command.Compile (compileIOWithOptions, printCompilationResult)
import Wasp.Cli.Command.Message (cliSendMessageC)
import Wasp.Cli.Command.Require (InWaspProject (InWaspProject), require)
import qualified Wasp.Cli.Common as Common
import Wasp.Cli.Message (cliSendMessage)
import Wasp.CompileOptions (CompileOptions (..))
import qualified Wasp.Generator
import Wasp.Generator.Monad (GeneratorWarning (GeneratorNeedsMigrationWarning))
import qualified Wasp.Message as Msg
import Wasp.Project (CompileError, CompileWarning)

-- | Builds Wasp project that the current working directory is part of.
-- Does all the steps, from analysis to generation, and at the end writes generated code
-- to the disk, to the .wasp/build dir.
-- At the end, prints a report on how building went (by printing warnings, errors,
-- success/failure message, further steps, ...).
-- Finally, throws if there was a compile/build error.
-- Very similar to 'compile'.
build :: Command ()
build = do
  InWaspProject waspProjectDir <- require
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
  (warnings, errors) <- liftIO $ buildIO waspProjectDir buildDir

  liftIO $ printCompilationResult (warnings, errors)
  if null errors
    then do
      cliSendMessageC $
        Msg.Success "Your wasp project has been successfully built! Check it out in the .wasp/build directory."
    else
      throwError $
        CommandError "Building of wasp project failed" $ show (length errors) ++ " errors found"

buildIO ::
  Path' Abs (Dir Common.WaspProjectDir) ->
  Path' Abs (Dir Wasp.Generator.ProjectRootDir) ->
  IO ([CompileWarning], [CompileError])
buildIO waspProjectDir buildDir = compileIOWithOptions options waspProjectDir buildDir
  where
    options =
      CompileOptions
        { externalClientCodeDirPath = waspProjectDir </> Common.extClientCodeDirInWaspProjectDir,
          externalServerCodeDirPath = waspProjectDir </> Common.extServerCodeDirInWaspProjectDir,
          externalSharedCodeDirPath = waspProjectDir </> Common.extSharedCodeDirInWaspProjectDir,
          isBuild = True,
          sendMessage = cliSendMessage,
          -- Ignore "DB needs migration warnings" during build, as that is not a required step.
          generatorWarningsFilter =
            filter
              ( \case
                  GeneratorNeedsMigrationWarning _ -> False
                  _ -> True
              )
        }
