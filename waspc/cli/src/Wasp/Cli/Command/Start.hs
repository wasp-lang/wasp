module Wasp.Cli.Command.Start
  ( start,
  )
where

import Data.Text (Text, pack)
import qualified Data.Text as Text
import Data.Text.IO (writeFile)
import Data.String.AnsiEscapeCodes.Strip.Text (stripAnsiEscapeCodes)
import Data.Time (getCurrentTime)
import System.Directory (renamePath)
import Control.Concurrent (Chan, dupChan, forkIO, newChan, readChan, threadDelay)
import Control.Concurrent.Async (concurrently, race)
import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import StrongPath ((</>))
import Wasp.Cli.Command (Command, CommandError (..))
import Wasp.Cli.Command.Common
  ( findWaspProjectRootDirFromCwd,
  )
import Wasp.Cli.Command.Compile
  ( compileIO,
  )
import Wasp.Cli.Command.Message (cliSendMessageC)
import Wasp.Cli.Command.Watch (watch)
import qualified Wasp.Cli.Common as Common
import qualified Wasp.Lib
import qualified Wasp.Generator.Job as Job
import Wasp.Generator.Job (JobMessage)
import qualified Wasp.Message as Msg

-- | Does initial compile of wasp code and then runs the generated project.
-- It also listens for any file changes and recompiles and restarts generated project accordingly.
start :: Command ()
start = do
  chan <- newChan
  waspRoot <- findWaspProjectRootDirFromCwd
  let outDir = waspRoot </> Common.dotWaspDirInWaspProjectDir </> Common.generatedCodeDirInDotWaspDir

  cliSendMessageC $ Msg.Start "Starting compilation and setup phase. Hold tight..."

  compilationResult <- liftIO $ compileIO waspRoot outDir
  case compilationResult of
    Left compileError -> throwError $ CommandError "Compilation failed" compileError
    Right () -> cliSendMessageC $ Msg.Success "Code has been successfully compiled, project has been generated."

  cliSendMessageC $ Msg.Start "Listening for file changes..."
  cliSendMessageC $ Msg.Start "Starting up generated project..."

  watchOrStartResult <- liftIO $ race (watch waspRoot outDir) (Wasp.Lib.start outDir)
  case watchOrStartResult of
    Left () -> error "This should never happen, listening for file changes should never end but it did."
    Right startResult -> case startResult of
      Left startError -> throwError $ CommandError "Start failed" startError
      Right () -> error "This should never happen, start should never end but it did."


writeAppOutputToHtml :: Chan JobMessage -> IO ()
writeAppOutputToHtml chan = do
  _ <- forkIO $ writeOutput chan []
  return ()

writeOutput :: Chan JobMessage -> [JobMessage] -> IO ()
writeOutput chan jobMessages = do
  messageOrDelay <- race (readChan chan) (threadDelaySeconds 3)
  messages <-
    case messageOrDelay of
      Left jobMessage -> return (jobMessage : jobMessages)
      Right _timerExpired -> return jobMessages
  writeOutputFile messages
  writeOutput chan messages
  where
    writeOutputFile :: [JobMessage] -> IO ()
    writeOutputFile messages = do
      timestamp <- getCurrentTime
      Data.Text.IO.writeFile "/tmp/test.html.tmp" (htmlShell (show timestamp) messages)
      renamePath "/tmp/test.html.tmp" "/tmp/test.html"

    threadDelaySeconds :: Int -> IO ()
    threadDelaySeconds =
      let microsecondsInASecond = 1000000
       in threadDelay . (* microsecondsInASecond)

htmlShell :: String -> [JobMessage] -> Text
htmlShell timestamp jobMessages =
  Text.unwords
    [ "<html><head>",
      "<title>Wasp Powerline</title>",
      "<style> div.scrollable { height: 20%; overflow-y: scroll; border: 1px solid black; display: flex; flex-direction: column-reverse; } </style>",
      "<script>let shouldRefresh = true;</script>",
      "<script>function disableRefresh() { shouldRefresh = false; document.getElementById('refreshButton').style.display = 'none';  }</script>",
      "</head>",
      "<body>",
      "<div><p>Last write timestamp: " <> pack timestamp <> "</p></div>",
      "<div><p>Last JS refresh timestamp: <span id='jsTime'></span><button id='refreshButton' onclick='disableRefresh();'>Disable Refresh</button></p></div>",
      "<div class='logContainer'>" <> splitJobMessages <> "</div>",
      "<script>setTimeout(() => { shouldRefresh && location.reload() }, 3000)</script>",
      "<script>document.getElementById('jsTime').innerHTML = new Date();</script>",
      "</body>",
      "</html>"
    ]
  where
    splitJobMessages :: Text
    splitJobMessages =
      let webAppMessages = filter (\jm -> Job._jobType jm == Job.WebApp) jobMessages
          serverMessages = filter (\jm -> Job._jobType jm == Job.Server) jobMessages
          dbMessages = filter (\jm -> Job._jobType jm == Job.Db) jobMessages
       in makeMessagesPretty webAppMessages "Web"
            <> makeMessagesPretty serverMessages "Server"
            <> makeMessagesPretty dbMessages "Db"

    makeMessagesPretty :: [JobMessage] -> Text -> Text
    makeMessagesPretty jms title =
      Text.unwords
        [ "<h2>" <> title <> "</h2>",
          "<div class='scrollable'>",
          Text.intercalate "\n" $ map makeMessagePretty jms,
          "</div>",
          "<hr/>"
        ]

    -- TODO: Hacky, fix this cleanup some
    makeMessagePretty :: JobMessage -> Text
    makeMessagePretty jm =
      case Job._data jm of
        Job.JobOutput txt _ -> "<div>" <> (Text.intercalate "<br/>" . Text.split (== '\n') . Text.strip . stripAnsiEscapeCodes $ txt) <> "</div>"
        Job.JobExit _ -> ""
