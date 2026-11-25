module Wasp.Cli.Console.Internal.TerminalReporterTest where

import Test.Hspec
import Wasp.Cli.Console.Internal.OutputStyle (OutputStyle (..))
import Wasp.Cli.Console.Internal.TerminalReporter (renderMessage, renderTaskFailure, renderTaskStart)
import qualified Wasp.Message as Msg
import Wasp.Util.TextStyle (styleCode)

spec_renderMessage :: Spec
spec_renderMessage = do
  describe "in the plain output style" $ do
    it "shows an info as it is" $ do
      renderMessage Plain (Msg.Info "Listening for file changes...")
        `shouldBe` "Listening for file changes..."

    it "marks a success with a check" $ do
      renderMessage Plain (Msg.Success "Compiled the project")
        `shouldBe` "✔ Compiled the project"

    it "labels an error and indents its details" $ do
      renderMessage Plain (Msg.Failure "Cannot connect to the database" "It needs to be running.\nStart it first.")
        `shouldBe` "! ERROR: Cannot connect to the database\n  It needs to be running.\n  Start it first."

    it "labels a warning" $ do
      renderMessage Plain (Msg.Warning "You have unapplied migrations" "")
        `shouldBe` "! WARNING: You have unapplied migrations"

    it "keeps the backticks of code and drops its colour" $ do
      renderMessage Plain (Msg.Info $ "Run " ++ styleCode "wasp install" ++ ".")
        `shouldBe` "Run `wasp install`."

  describe "in the styled output style" $ do
    it "shows code in yellow" $ do
      renderMessage Styled (Msg.Info $ "Run " ++ styleCode "wasp install" ++ ".")
        `shouldBe` "Run \ESC[33m`wasp install`\ESC[39m."

    it "puts the label of an error on a red background and makes its headline bold" $ do
      renderMessage Styled (Msg.Failure "Compilation failed" "")
        `shouldBe` "\ESC[31m!\ESC[0m \ESC[31m\ESC[7m\ESC[1m ERROR \ESC[0m \ESC[1mCompilation failed\ESC[0m"

spec_renderTaskStart :: Spec
spec_renderTaskStart = do
  it "marks a started task with a triangle" $ do
    renderTaskStart Plain "Compiling the project" `shouldBe` "▶ Compiling the project"

spec_renderTaskFailure :: Spec
spec_renderTaskFailure = do
  it "marks a failed task with a cross" $ do
    renderTaskFailure Plain "Failed to build the SDK" `shouldBe` "✖ Failed to build the SDK"
