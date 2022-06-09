module Wasp.Cli.Command.Compile
  ( compileIO,
    compile,
    compileIOWithOptions,
  )
where

import Control.Concurrent (Chan, writeChan)
import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import Data.List (intercalate)
import StrongPath (Abs, Dir, Path', (</>))
import Wasp.Cli.Command (Command, CommandError (..))
import Wasp.Cli.Command.Common
  ( findWaspProjectRootDirFromCwd,
  )
import Wasp.Cli.Command.Message (cliSendMessageC)
import qualified Wasp.Cli.Common as Common
import Wasp.Cli.Message (cliSendMessage)
import Wasp.Common (WaspProjectDir)
import Wasp.CompileOptions (CompileOptions (..))
import qualified Wasp.Lib
import qualified Wasp.Message as Msg

compile :: Command ()
compile = do
  waspProjectDir <- findWaspProjectRootDirFromCwd
  let outDir =
        waspProjectDir </> Common.dotWaspDirInWaspProjectDir
          </> Common.generatedCodeDirInDotWaspDir

  cliSendMessageC $ Msg.Start "Compiling wasp code..."
  compilationResult <- liftIO $ compileIO waspProjectDir outDir Nothing
  case compilationResult of
    Left compileError -> throwError $ CommandError "Compilation failed" compileError
    Right () -> cliSendMessageC $ Msg.Success "Code has been successfully compiled, project has been generated."

-- | Compiles Wasp source code in waspProjectDir directory and generates a project
--   in given outDir directory.
compileIO ::
  Path' Abs (Dir WaspProjectDir) ->
  Path' Abs (Dir Wasp.Lib.ProjectRootDir) ->
  Maybe (Chan String) ->
  IO (Either String ())
compileIO waspProjectDir outDir chan = compileIOWithOptions options waspProjectDir outDir
  where
    options =
      CompileOptions
        { externalCodeDirPath = waspProjectDir </> Common.extCodeDirInWaspProjectDir,
          isBuild = False,
          sendMessage = forwardMessage chan
        }
    forwardMessage :: Maybe (Chan String) -> Msg.SendMessage
    forwardMessage Nothing = cliSendMessage
    forwardMessage (Just c) =
      \m -> do
        writeChan c (show m)
        cliSendMessage m

-- TODO: capture warnings/errors like above
compileIOWithOptions ::
  CompileOptions ->
  Path' Abs (Dir Common.WaspProjectDir) ->
  Path' Abs (Dir Wasp.Lib.ProjectRootDir) ->
  IO (Either String ())
compileIOWithOptions options waspProjectDir outDir = do
  (compilerWarnings, compilerErrors) <- Wasp.Lib.compile waspProjectDir outDir options
  case compilerErrors of
    [] -> do
      displayWarnings compilerWarnings
      return $ Right ()
    errors -> return $ Left $ formatMessages errors
  where
    formatMessages messages = intercalate "\n" $ map ("- " ++) messages
    displayWarnings [] = return ()
    displayWarnings warnings =
      cliSendMessage $ Msg.Warning "Your project compiled with warnings" (formatMessages warnings ++ "\n\n")
