module Job.IO.PrefixedWriterTest where

import Control.Exception (bracket, finally)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Text as T
import qualified Data.Text.IO as T.IO
import GHC.IO.Handle (hDuplicate, hDuplicateTo)
import System.Directory (getTemporaryDirectory, removeFile)
import System.IO (IOMode (ReadMode), hClose, hFlush, openTempFile, stdout, withFile)
import Test.Hspec
import qualified Wasp.Job as J
import Wasp.Job.IO.PrefixedWriter (printJobMessagePrefixed, runPrefixedWriter)
import qualified Wasp.Util.Terminal as Term

spec_prefixedOutput :: Spec
spec_prefixedOutput = do
  -- Capture while building the spec, before Tasty starts parallel tests.
  statusOutput <- runIO $ captureStdout $ runPrefixedWriter $ do
    printJobMessagePrefixed $ dbOutput "ready\n"
    liftIO $ T.IO.putStr "PostgreSQL ready.\n"
  it "ends the log line before an ordinary status message" $
    statusOutput `shouldBe` "\n" <> dbPrefix <> "ready\nPostgreSQL ready.\n"

  emptyLinesOutput <- runIO $ captureStdout $ runPrefixedWriter $ do
    printJobMessagePrefixed $ dbOutput "first\n"
    printJobMessagePrefixed $ dbOutput "\nlast\n"
  it "labels empty lines, including ones received separately" $
    emptyLinesOutput `shouldBe` "\n" <> dbPrefix <> "first\n" <> dbPrefix <> "\n" <> dbPrefix <> "last\n"

  partialLinesOutput <- runIO $ captureStdout $ runPrefixedWriter $ do
    printJobMessagePrefixed $ dbOutput "first"
    printJobMessagePrefixed $ dbOutput " line\n"
    printJobMessagePrefixed $ dbOutput "second\n"
  it "continues partial lines and leaves no trailing prefix" $
    partialLinesOutput `shouldBe` "\n" <> dbPrefix <> "first line\n" <> dbPrefix <> "second\n"

  switchedJobsOutput <- runIO $ captureStdout $ runPrefixedWriter $ do
    printJobMessagePrefixed $ dbOutput "first\n"
    printJobMessagePrefixed $ J.JobMessage (J.JobOutput "second\n" J.Stdout) J.Server
    printJobMessagePrefixed $ dbOutput "third\n"
  it "does not add a second newline when the job changes" $
    switchedJobsOutput `shouldBe` "\n" <> dbPrefix <> "first\n" <> serverPrefix <> "second\n" <> dbPrefix <> "third\n"

  carriageReturnOutput <- runIO $ captureStdout $ runPrefixedWriter $ do
    printJobMessagePrefixed $ dbOutput "one\r"
    printJobMessagePrefixed $ dbOutput "\ntwo\n"
  it "preserves carriage-return labeling across separate writes" $
    carriageReturnOutput `shouldBe` "\n" <> dbPrefix <> "one\r" <> dbPrefix <> "\n" <> dbPrefix <> "two\n"

captureStdout :: IO () -> IO T.Text
captureStdout action = do
  temporaryDirectory <- getTemporaryDirectory
  bracket (openTempFile temporaryDirectory "wasp-output") cleanup $ \(path, handle) -> do
    bracket (hDuplicate stdout) restoreStdout $ \_ -> do
      hDuplicateTo handle stdout
      action `finally` hFlush stdout
    hClose handle
    withFile path ReadMode T.IO.hGetContents
  where
    cleanup (path, handle) = hClose handle `finally` removeFile path
    restoreStdout original = hDuplicateTo original stdout `finally` hClose original

dbOutput :: T.Text -> J.JobMessage
dbOutput text = J.JobMessage (J.JobOutput text J.Stdout) J.Db

dbPrefix, serverPrefix :: T.Text
dbPrefix = T.pack $ Term.applyStyles [Term.Blue] "[" <> "   " <> Term.applyStyles [Term.Blue] "Db" <> "   " <> Term.applyStyles [Term.Blue] "]" <> " "
serverPrefix = T.pack $ Term.applyStyles [Term.Magenta] "[" <> " " <> Term.applyStyles [Term.Magenta] "Server" <> " " <> Term.applyStyles [Term.Magenta] "]" <> " "
