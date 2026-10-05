module Job.IO.PrefixedWriterTest where

import qualified Data.Text as T
import Test.Hspec
import qualified Wasp.Job as J
import Wasp.Job.IO.PrefixedWriter (formatJobMessage)
import qualified Wasp.Util.Terminal as Term

spec_prefixedOutput :: Spec
spec_prefixedOutput = do
  it "emits the newline before an ordinary status message" $ do
    formatJobMessage Nothing (dbOutput "ready\n") <> "PostgreSQL ready.\n"
      `shouldBe` dbPrefix <> "ready\nPostgreSQL ready.\n"

  it "continues partial output without adding a prefix" $ do
    formatJobMessage Nothing (dbOutput "Migration name: ") `shouldBe` dbPrefix <> "Migration name: "
    formatJobMessage (Just $ dbOutput "Migration name: ") (dbOutput "answer\n") `shouldBe` "answer\n"

  it "adds the next prefix only when content follows the newline" $ do
    formatJobMessage (Just $ dbOutput "first\n") (dbOutput "second\n") `shouldBe` dbPrefix <> "second\n"

  it "preserves blank lines without unused prefixes" $ do
    formatJobMessage Nothing (dbOutput "first\n\nlast\n") `shouldBe` dbPrefix <> "first\n\n" <> dbPrefix <> "last\n"
    formatJobMessage (Just $ dbOutput "first") (dbOutput "") `shouldBe` ""

  it "preserves carriage returns and CRLF, including split chunks" $ do
    formatJobMessage Nothing (dbOutput "one\rtwo\r\n") `shouldBe` dbPrefix <> "one\r" <> dbPrefix <> "two\r\n"
    formatJobMessage (Just $ dbOutput "one\r") (dbOutput "\ntwo\n") `shouldBe` "\n" <> dbPrefix <> "two\n"

  it "separates different jobs only when the previous line is incomplete" $ do
    let serverOutput text = J.JobMessage (J.JobOutput text J.Stdout) J.Server
    formatJobMessage (Just $ serverOutput "partial") (dbOutput "next\n") `shouldBe` "\n" <> dbPrefix <> "next\n"
    formatJobMessage (Just $ serverOutput "working\r") (dbOutput "next\n") `shouldBe` "\n" <> dbPrefix <> "next\n"
    formatJobMessage (Just $ serverOutput "complete\n") (dbOutput "next\n") `shouldBe` dbPrefix <> "next\n"

  it "separates stdout and stderr from the same job" $ do
    let errorOutput text = J.JobMessage (J.JobOutput text J.Stderr) J.Db
    formatJobMessage (Just $ errorOutput "partial") (dbOutput "next\n") `shouldBe` "\n" <> dbPrefix <> "next\n"
    formatJobMessage (Just $ errorOutput "complete\n") (dbOutput "next\n") `shouldBe` dbPrefix <> "next\n"

  it "preserves colored output and its trailing newline" $ do
    formatJobMessage Nothing (dbOutput "\ESC[32mready\ESC[0m\n") `shouldBe` dbPrefix <> "\ESC[32mready\ESC[0m\n"

dbOutput :: T.Text -> J.JobMessage
dbOutput text = J.JobMessage (J.JobOutput text J.Stdout) J.Db

dbPrefix :: T.Text
dbPrefix = T.pack $ Term.applyStyles [Term.Blue] "[" <> "   " <> Term.applyStyles [Term.Blue] "Db" <> "   " <> Term.applyStyles [Term.Blue] "]" <> " "
