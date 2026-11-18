module Job.IO.PrefixedWriterTest where

import Data.List (mapAccumL)
import qualified Data.Text as T
import Test.Hspec
import qualified Wasp.Job as J
import Wasp.Job.IO.PrefixedWriter (Output (..), formatOutput, initialPrefixedWriterState)
import qualified Wasp.Util.Terminal as Term

spec_prefixedOutput :: Spec
spec_prefixedOutput = do
  it "prints the final newline without waiting for another message" $
    formatMessages [dbOutput "ready\n"]
      `shouldBe` dbPrefix <> "ready\n"

  it "labels empty lines, including ones received separately" $
    formatMessages [dbOutput "first\n", dbOutput "\nlast\n"]
      `shouldBe` dbPrefix <> "first\n" <> dbPrefix <> "\n" <> dbPrefix <> "last\n"

  it "continues partial lines and leaves no trailing prefix" $
    formatMessages [dbOutput "first", dbOutput " line\n", dbOutput "second\n"]
      `shouldBe` dbPrefix <> "first line\n" <> dbPrefix <> "second\n"

  it "does not add a second newline when the job changes" $
    formatMessages
      [ dbOutput "first\n",
        (Output J.Server J.Stdout, "second\n"),
        dbOutput "third\n"
      ]
      `shouldBe` dbPrefix <> "first\n" <> serverPrefix <> "second\n" <> dbPrefix <> "third\n"

  it "labels a leading empty line when the previous job's line is complete" $
    formatMessages [(Output J.Server J.Stdout, "first\n"), dbOutput "\nsecond\n"]
      `shouldBe` serverPrefix <> "first\n" <> dbPrefix <> "\n" <> dbPrefix <> "second\n"

  it "lets a leading newline finish the previous job's partial line" $
    formatMessages [(Output J.Server J.Stdout, "partial"), dbOutput "\nsecond\n"]
      `shouldBe` serverPrefix <> "partial" <> "\n" <> dbPrefix <> "second\n"

  it "preserves carriage-return labeling across separate writes" $
    formatMessages [dbOutput "one\r", dbOutput "\ntwo\n"]
      `shouldBe` dbPrefix <> "one\r" <> dbPrefix <> "\n" <> dbPrefix <> "two\n"

formatMessages :: [(Output, T.Text)] -> T.Text
formatMessages = T.concat . snd . mapAccumL (uncurry . formatOutput "15:18:42") initialPrefixedWriterState

dbOutput :: T.Text -> (Output, T.Text)
dbOutput text = (Output J.Db J.Stdout, text)

dbPrefix :: T.Text
dbPrefix = T.pack $ Term.applyStyles [Term.Grey] "15:18:42" <> " " <> Term.applyStyles [Term.Blue] "[db]" <> " "

serverPrefix :: T.Text
serverPrefix = T.pack $ Term.applyStyles [Term.Grey] "15:18:42" <> " " <> Term.applyStyles [Term.Magenta] "[server]" <> " "
