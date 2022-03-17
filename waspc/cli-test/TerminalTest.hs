module TerminalTest where

import Test.Tasty.Hspec
import Wasp.Cli.Terminal (asWaspSuccessMessage)

spec_terminalMessages :: Spec
spec_terminalMessages = do
  it "it can format messages" $ do
    asWaspSuccessMessage "Hello, world!" `shouldBe` "\n✅ --- Hello, world! --------------------------------------------------------------\n"
