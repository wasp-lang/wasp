module Job.IO.PrefixedWriterTest where

import Data.List (mapAccumL)
import qualified Data.Text as T
import Test.Hspec
import qualified Wasp.Job as J
import Wasp.Job.IO.PrefixedWriter (formatJobMessage, initialPrefixedWriterState)
import qualified Wasp.Util.Terminal as Term

spec_prefixedOutput :: Spec
spec_prefixedOutput = do
  it "prints the final newline without waiting for another message" $
    formatMessages [dbOutput "ready\n"]
      `shouldBe` "\n" <> dbPrefix <> "ready\n"

  it "labels empty lines, including ones received separately" $
    formatMessages [dbOutput "first\n", dbOutput "\nlast\n"]
      `shouldBe` "\n" <> dbPrefix <> "first\n" <> dbPrefix <> "\n" <> dbPrefix <> "last\n"

  it "continues partial lines and leaves no trailing prefix" $
    formatMessages [dbOutput "first", dbOutput " line\n", dbOutput "second\n"]
      `shouldBe` "\n" <> dbPrefix <> "first line\n" <> dbPrefix <> "second\n"

  it "does not add a second newline when the job changes" $
    formatMessages
      [ dbOutput "first\n",
        J.JobMessage (J.JobOutput "second\n" J.Stdout) J.Server,
        dbOutput "third\n"
      ]
      `shouldBe` "\n" <> dbPrefix <> "first\n" <> serverPrefix <> "second\n" <> dbPrefix <> "third\n"

  it "preserves carriage-return labeling across separate writes" $
    formatMessages [dbOutput "one\r", dbOutput "\ntwo\n"]
      `shouldBe` "\n" <> dbPrefix <> "one\r" <> dbPrefix <> "\n" <> dbPrefix <> "two\n"

formatMessages :: [J.JobMessage] -> T.Text
formatMessages = T.concat . snd . mapAccumL formatJobMessage initialPrefixedWriterState

dbOutput :: T.Text -> J.JobMessage
dbOutput text = J.JobMessage (J.JobOutput text J.Stdout) J.Db

dbPrefix :: T.Text
dbPrefix = T.pack $ Term.applyStyles [Term.Blue] "[" <> "   " <> Term.applyStyles [Term.Blue] "Db" <> "   " <> Term.applyStyles [Term.Blue] "]" <> " "

serverPrefix :: T.Text
serverPrefix = T.pack $ Term.applyStyles [Term.Magenta] "[" <> " " <> Term.applyStyles [Term.Magenta] "Server" <> " " <> Term.applyStyles [Term.Magenta] "]" <> " "
