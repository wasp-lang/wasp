module Wasp.Cli.Console.CapabilitiesTest where

import Test.Hspec
import Wasp.Cli.Console.Capabilities (Capabilities (..), Surroundings (..), decideCapabilities)

spec_decideCapabilities :: Spec
spec_decideCapabilities = do
  it "styles the output and prompts in a normal terminal" $ do
    decideCapabilities normalTerminal
      `shouldBe` Capabilities {canStyleOutput = True, canPrompt = True}

  it "doesn't style the output or prompt when the output goes to a file (`wasp start > log.txt`)" $ do
    decideCapabilities normalTerminal {isOutputTerminal = False}
      `shouldBe` Capabilities {canStyleOutput = False, canPrompt = False}

  it "styles the output but doesn't prompt when the input is piped in (`echo y | wasp uninstall`)" $ do
    decideCapabilities normalTerminal {isInputTerminal = False}
      `shouldBe` Capabilities {canStyleOutput = True, canPrompt = False}

  it "prompts but doesn't style the output in a dumb terminal" $ do
    decideCapabilities normalTerminal {terminalType = Just "dumb"}
      `shouldBe` Capabilities {canStyleOutput = False, canPrompt = True}

  it "prompts but doesn't style the output when NO_COLOR is set" $ do
    decideCapabilities normalTerminal {noColor = Just "1"}
      `shouldBe` Capabilities {canStyleOutput = False, canPrompt = True}

  it "ignores an empty NO_COLOR" $ do
    decideCapabilities normalTerminal {noColor = Just ""}
      `shouldBe` Capabilities {canStyleOutput = True, canPrompt = True}

  it "doesn't style the output or prompt on CI, even in a terminal" $ do
    decideCapabilities normalTerminal {isOnCi = True}
      `shouldBe` Capabilities {canStyleOutput = False, canPrompt = False}
  where
    normalTerminal =
      Surroundings
        { isInputTerminal = True,
          isOutputTerminal = True,
          isOnCi = False,
          terminalType = Just "xterm-256color",
          noColor = Nothing
        }
