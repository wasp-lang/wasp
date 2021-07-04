-- | Unit tests for `listenForEvents` from the Command.Watch module
module Cli.Command.WatchTest where

import qualified Command.Watch
import Test.Tasty.Hspec

spec_ListenForEvents :: Spec
spec_ListenForEvents = do
  describe "Command.Watch.listenForEvents" $ do
    it "testing tests" $ do
      {-
        TODO: The test cases that need to be added:
         1. simulate an event to occur within the next second and see that `recompile` runs only once
         2. simulate an event to occur after a second and see that `recompile` runs immediately      
      -}
      2 `shouldBe` 2
