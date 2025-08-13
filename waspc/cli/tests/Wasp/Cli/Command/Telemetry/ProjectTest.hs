module Wasp.Cli.Command.Telemetry.ProjectTest where

import Test.Tasty.Hspec
import Wasp.Cli.Command.Telemetry.Project
  ( checkIfEnvValueIsTruthy,
  )

spec_checkIfEnvValueIsTruthy :: Spec
spec_checkIfEnvValueIsTruthy = do
  it "Correctly determines if different env values are truthy" $ do
    let testCases =
          [ (Nothing, False),
            (Just "", False),
            (Just "false", False),
            (Just "False", False),
            (Just "FALSE", False),
            (Just "true", True),
            (Just "something", True),
            (Just "0", True),
            (Just "1", True),
            (Just "falsy", True),
            (Just "foo", True)
          ]
    checkIfEnvValueIsTruthy . fst <$> testCases
      `shouldBe` snd <$> testCases
