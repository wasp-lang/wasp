module Wasp.Cli.Command.Start
  ( start,
    parseStart,
  )
where

import Control.Concurrent ()
import Control.Concurrent.Async (race)
import Control.Concurrent.MVar (MVar, newMVar, tryTakeMVar)
import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import Data.Maybe (fromMaybe)
import qualified Options.Applicative as O
import StrongPath ((</>))
import Wasp.Cli.Command (Command, CommandError (..))
import Wasp.Cli.Command.Call (Call (Start), StartArg (..))
import Wasp.Cli.Command.Common (findWaspProjectRootDirFromCwd)
import Wasp.Cli.Command.Compile (compile, printWarningsAndErrorsIfAny)
import Wasp.Cli.Command.Message (cliSendMessageC)
import qualified Wasp.Cli.Command.Start.Db
import Wasp.Cli.Command.Watch (watch)
import qualified Wasp.Cli.Common as Common
import Wasp.Cli.Parser.Util (mkCommand)
import qualified Wasp.Generator
import qualified Wasp.Message as Msg
import Wasp.Project (CompileError, CompileWarning)

parseStartDb :: O.Parser (Maybe StartArg)
parseStartDb =
  O.optional $
    O.subparser $
      mkCommand "db" (pure StartDb) "This will launch the development PostgreSQL database."

parseStart :: O.Parser Call
parseStart = Start <$> starter
  where
    starter = fromMaybe StartNormal <$> parseStartDb

start :: StartArg -> Command ()
start arg = case arg of
  StartDb -> Wasp.Cli.Command.Start.Db.start
  StartNormal -> startNormal

-- | Does initial compile of wasp code and then runs the generated project.
-- It also listens for any file changes and recompiles and restarts generated project accordingly.
startNormal :: Command ()
startNormal = do
  waspRoot <- findWaspProjectRootDirFromCwd
  let outDir = waspRoot </> Common.dotWaspDirInWaspProjectDir </> Common.generatedCodeDirInDotWaspDir

  cliSendMessageC $ Msg.Start "Starting compilation and setup phase. Hold tight..."

  warnings <- compile

  cliSendMessageC $ Msg.Start "Listening for file changes..."
  cliSendMessageC $ Msg.Start "Starting up generated project..."

  watchOrStartResult <- liftIO $ do
    -- This MVar is used to exchange information between the two processes below running in
    -- parallel, specifically to allow us to pass the results of re-compilation done by 'watch'
    -- into the 'onJobsQuietDown' handler used by 'startWebApp'.
    -- This way we can show newest Wasp compile warnings and errors (produced by recompilation from
    -- 'watch') once jobs from 'start' quiet down a bit.
    ongoingCompilationResultMVar <- newMVar (warnings, [])
    let watchWaspProjectSource = watch waspRoot outDir ongoingCompilationResultMVar
    let startGeneratedWebApp = Wasp.Generator.start outDir (onJobsQuietDown ongoingCompilationResultMVar)
    -- In parallel:
    -- 1. watch for any changes in the Wasp project, be it users wasp code or users JS/HTML/...
    --    code. On any change, Wasp is recompiled (and generated code is re-generated).
    -- 2. start web app in dev mode, which will then also watch for changes but in the generated
    --    code, and will also react to them by restarting the web app.
    -- Both of these should run forever, unless some super serious error happens.
    watchWaspProjectSource `race` startGeneratedWebApp

  case watchOrStartResult of
    Left () -> error "This should never happen, listening for file changes should never end but it did."
    Right startResult -> case startResult of
      Left startError -> throwError $ CommandError "Start failed" startError
      Right () -> error "This should never happen, start should never end but it did."
  where
    onJobsQuietDown :: MVar ([CompileWarning], [CompileError]) -> IO ()
    onJobsQuietDown ongoingCompilationResultMVar = do
      -- Once jobs from generated web app quiet down a bit, we print any warnings / errors from the
      -- latest (re)compile that haven't yet been printed in this situation.
      -- This way we ensure that even if web app jobs print a lot of output, users
      -- won't miss wasp compiler warnings and errors, since they will be again printed after all
      -- of that output.
      maybeOngoingCompilationResult <- tryTakeMVar ongoingCompilationResultMVar
      case maybeOngoingCompilationResult of
        Nothing -> return ()
        Just ([], []) -> return ()
        Just (warnings, errors) -> do
          putStrLn ""
          printWarningsAndErrorsIfAny (warnings, errors)
          putStrLn ""
