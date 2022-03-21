module TerminalTest where

import Test.Tasty.Hspec
import Wasp.Cli.Terminal
  ( asWaspMessage,
  )

-- TODO: Showing it can be done. Remove me when writing first real CLI test.
spec_terminalMessages :: Spec
spec_terminalMessages = do
  it "can format messages of various kinds" $ do
    asWaspMessage "Hello, world!" `shouldBe` "\n --- Hello, world! ---------------------------------------------------------------\n"
