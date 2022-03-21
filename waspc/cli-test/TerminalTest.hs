module TerminalTest where

import Test.Tasty.Hspec
import Wasp.Cli.Terminal
  ( asWaspFailureMessage,
    asWaspMessage,
    asWaspStartMessage,
    asWaspSuccessMessage,
    asWaspWarningMessage,
  )

spec_terminalMessages :: Spec
spec_terminalMessages = do
  it "it can format messages" $ do
    asWaspMessage "Hello, world!" `shouldBe` "\n --- Hello, world! ---------------------------------------------------------------\n"
    asWaspStartMessage "Hello, world!" `shouldBe` "\n🐝 --- Hello, world! --------------------------------------------------------------\n"
    asWaspSuccessMessage "Hello, world!" `shouldBe` "\n✅ --- Hello, world! --------------------------------------------------------------\n"
    asWaspWarningMessage "Hello, world!" `shouldBe` "\n\n👀 --- [Warning] Hello, world! ----------------------------------------------------\n\n"
    asWaspFailureMessage "Hello, world!" `shouldBe` "\n\n❌ --- [Error] Hello, world! ------------------------------------------------------\n\n"
