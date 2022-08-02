module Wasp.Generator.Job.IO
  ( readJobMessagesAndReturnTextOutput,
    readJobMessagesAndPrintThemPrefixed,
    printPrefixedJobMessage,
    printJobMessage,
  )
where

import Control.Concurrent (Chan, readChan)
import qualified Data.Text as T
import qualified Data.Text.IO as T.IO
import System.Exit (ExitCode (..))
import System.IO (Handle, hFlush, stderr, stdout)
import qualified Wasp.Generator.Job as J
import qualified Wasp.Util.Terminal as Term

readJobMessagesAndReturnTextOutput :: Chan J.JobMessage -> IO [T.Text]
readJobMessagesAndReturnTextOutput =
  let go jobMessages chan = do
        jobMsg <- readChan chan
        case J._data jobMsg of
          J.JobOutput output _ -> go (output : jobMessages) chan
          J.JobExit {} -> return . reverse $ jobMessages
   in go []

readJobMessagesAndPrintThemPrefixed :: Chan J.JobMessage -> IO ()
readJobMessagesAndPrintThemPrefixed =
  let go prevJobMsg chan = do
        jobMsg <- readChan chan
        case J._data jobMsg of
          J.JobOutput {} -> printPrefixedJobMessage prevJobMsg jobMsg >> go (Just jobMsg) chan
          J.JobExit {} -> return ()
   in go Nothing

printPrefixedJobMessage :: Maybe J.JobMessage -> J.JobMessage -> IO ()
printPrefixedJobMessage maybePrevJobMessage jobMessage = do
  let outHandle = getJobMessageOutHandle jobMessage
      prefix = makeJobMessagePrefix jobMessage
      content = getJobMessageContent jobMessage

  let maybeAddPrefixAtStart =
        ((if (J._jobType <$> maybePrevJobMessage) /= Just (J._jobType jobMessage) then "\n" <> prefix else "") <>)
      addPrefixAfterSubstr substr = T.intercalate (substr <> prefix) . T.splitOn substr
      addPrefix = maybeAddPrefixAtStart . addPrefixAfterSubstr "\n" . addPrefixAfterSubstr "\r"

  T.IO.hPutStr outHandle $ addPrefix content
  hFlush outHandle

printJobMessage :: J.JobMessage -> IO ()
printJobMessage jobMsg = do
  let outHandle = getJobMessageOutHandle jobMsg
  let message = getJobMessageContent jobMsg
  T.IO.hPutStr outHandle message
  hFlush outHandle

makeJobMessagePrefix :: J.JobMessage -> T.Text
makeJobMessagePrefix jobMsg =
  case J._jobType jobMsg of
    J.Server -> T.pack $ Term.applyStyles [Term.Magenta] "Server"
    J.WebApp -> T.pack $ Term.applyStyles [Term.Cyan] "Web app"
    J.Db -> T.pack $ Term.applyStyles [Term.White] "Db"
    <> (if getJobMessageOutHandle jobMsg == stderr then " (stderr)" else "")
    <> ": "

getJobMessageOutHandle :: J.JobMessage -> Handle
getJobMessageOutHandle jobMsg = case J._data jobMsg of
  J.JobOutput _ outputType ->
    case outputType of
      J.Stdout -> stdout
      J.Stderr -> stderr
  J.JobExit _ -> stdout

getJobMessageContent :: J.JobMessage -> T.Text
getJobMessageContent jobMsg = case J._data jobMsg of
  J.JobOutput output _ -> output
  J.JobExit ExitSuccess -> "Job exited successfully."
  J.JobExit (ExitFailure exitCode) -> T.pack $ "Job failed with exit code " <> show exitCode
