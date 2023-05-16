module Wasp.Cli.ParserTest where

-- NOTE: This is temporary location for this test file.

import Options.Applicative
import Test.Tasty.Hspec
import Wasp.Cli.Command.Call

spec_allCliParsers :: Spec
spec_allCliParsers = do
  it "version" $ do
    1 `shouldBe` 2