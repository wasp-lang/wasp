module Wasp.Cli.Command.Test
  ( test,
    parseTest,
  )
where

import Control.Concurrent.Async (race)
import Control.Concurrent.MVar (newMVar)
import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import qualified Options.Applicative as O
import StrongPath (Abs, Dir, (</>))
import StrongPath.Types (Path')
import Wasp.Cli.Command (Command, CommandError (..))
import Wasp.Cli.Command.Call (Call (Test), TestArgs (TestClient, TestServer))
import Wasp.Cli.Command.Common (findWaspProjectRootDirFromCwd)
import Wasp.Cli.Command.Compile (compile)
import Wasp.Cli.Command.Message (cliSendMessageC)
import Wasp.Cli.Command.Watch (watch)
import qualified Wasp.Cli.Common as Common
import Wasp.Cli.Parser.Util (CommandType (CTForwardOptions), mkWrapperCommand)
import qualified Wasp.Generator
import Wasp.Generator.Common (ProjectRootDir)
import qualified Wasp.Message as Msg

testRestArgs :: O.Parser String
testRestArgs =
  O.strArgument
    ( O.metavar "VITEST_ARGUMENTS"
        <> O.help "Extra arguments that will be passed to Vitest. See https://vitest.dev/guide/cli.html"
    )

parseTestArgs :: O.Parser TestArgs
parseTestArgs =
  O.subparser $
    mconcat
      [ mkWrapperCommand "client" CTForwardOptions (TestClient <$> O.many testRestArgs) "Run your app client tests.",
        mkWrapperCommand "server" CTForwardOptions (TestServer <$> O.many testRestArgs) "Run your app server tests."
      ]

parseTest :: O.Parser Call
parseTest = Test <$> parseTestArgs

test :: TestArgs -> Command ()
test (TestClient args) = watchAndTest $ Wasp.Generator.testWebApp args
test (TestServer _) = throwError $ CommandError "Invalid arguments" "Server testing not yet implemented."

watchAndTest :: (Path' Abs (Dir ProjectRootDir) -> IO (Either String ())) -> Command ()
watchAndTest testRunner = do
  waspRoot <- findWaspProjectRootDirFromCwd
  let outDir = waspRoot </> Common.dotWaspDirInWaspProjectDir </> Common.generatedCodeDirInDotWaspDir

  cliSendMessageC $ Msg.Start "Starting compilation and setup phase. Hold tight..."

  warnings <- compile

  cliSendMessageC $ Msg.Start "Watching for file changes and running tests ..."

  watchOrStartResult <- liftIO $ do
    ongoingCompilationResultMVar <- newMVar (warnings, [])
    let watchWaspProjectSource = watch waspRoot outDir ongoingCompilationResultMVar
    watchWaspProjectSource `race` testRunner outDir

  case watchOrStartResult of
    Left () -> error "This should never happen, listening for file changes should never end but it did."
    Right startResult -> case startResult of
      Left testError -> throwError $ CommandError "Test failed" testError
      Right () -> return ()
