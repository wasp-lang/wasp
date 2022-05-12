module Wasp.Generator.Start
  ( start,
  )
where

import Control.Concurrent (Chan, dupChan, forkIO, newChan, readChan, threadDelay)
import Control.Concurrent.Async (concurrently, race)
import Data.List (intercalate)
import Data.Text (unpack)
import Data.Time (getCurrentTime)
import StrongPath (Abs, Dir, Path')
import System.Directory (renamePath)
import Wasp.Generator.Common (ProjectRootDir)
import Wasp.Generator.Job (JobMessage)
import qualified Wasp.Generator.Job as Job
import Wasp.Generator.Job.IO (readJobMessagesAndPrintThemPrefixed)
import Wasp.Generator.ServerGenerator.Start (startServer)
import Wasp.Generator.WebAppGenerator.Start (startWebApp)

-- | This is a blocking action, that will start the processes that run web app and server.
--   It will run as long as one of those processes does not fail.
start :: Path' Abs (Dir ProjectRootDir) -> IO (Either String ())
start projectDir = do
  chan <- newChan
  -- TODO:
  -- Need to handle Wasp messages.
  -- Figure out where to write and how to automatically open.
  -- Have separate areas for warning/errors that we have seen.
  -- Can show some sort of app structure diagram, etc. (May require AppSpec)
  -- How to handle escape sequences in output?
  dupChan chan >>= writeAppOutputToHtml
  let runStartJobs = race (startServer projectDir chan) (startWebApp projectDir chan)
  (_, serverOrWebExitCode) <- concurrently (readJobMessagesAndPrintThemPrefixed chan) runStartJobs
  case serverOrWebExitCode of
    Left serverExitCode -> return $ Left $ "Server failed with exit code " ++ show serverExitCode ++ "."
    Right webAppExitCode -> return $ Left $ "Web app failed with exit code " ++ show webAppExitCode ++ "."

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
      writeFile "/tmp/test.html.tmp" (htmlShell (show timestamp) messages)
      renamePath "/tmp/test.html.tmp" "/tmp/test.html"

    threadDelaySeconds :: Int -> IO ()
    threadDelaySeconds =
      let microsecondsInASecond = 1000000
       in threadDelay . (* microsecondsInASecond)

htmlShell :: String -> [JobMessage] -> String
htmlShell timestamp jobMessages =
  unwords
    [ "<html><head>",
      "<title>Wasp Powerline</title>",
      "</head>",
      "<body>",
      "<div><p>Last write timestamp: " ++ timestamp ++ "</p></div>",
      "<div><p>Last JS refresh timestamp: <span id='jsTime'></span></p></div>",
      "<div class='logContainer'>" ++ splitJobMessages ++ "</div>",
      "<script>setTimeout(() => { location.reload() }, 3000)</script>",
      "<script>document.getElementById('jsTime').innerHTML = new Date();</script>",
      "</body>",
      "</html>"
    ]
  where
    splitJobMessages :: String
    splitJobMessages =
      let webAppMessages = filter (\jm -> Job._jobType jm == Job.WebApp) jobMessages
          serverMessages = filter (\jm -> Job._jobType jm == Job.Server) jobMessages
          dbMessages = filter (\jm -> Job._jobType jm == Job.Db) jobMessages
       in makeMessagesPretty webAppMessages "Web"
            ++ makeMessagesPretty serverMessages "Server"
            ++ makeMessagesPretty dbMessages "Db"

    makeMessagesPretty :: [JobMessage] -> String -> String
    makeMessagesPretty jms title =
      unwords
        [ "<div>",
          "<h2>" ++ title ++ "</h2>",
          intercalate "<br/>" $ map makeMessagePretty jms,
          "</div>"
        ]

    makeMessagePretty :: JobMessage -> String
    makeMessagePretty jm =
      case Job._data jm of
        Job.JobOutput txt _ -> unpack txt
        Job.JobExit _ -> ""
