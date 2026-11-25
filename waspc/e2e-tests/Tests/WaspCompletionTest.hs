module Tests.WaspCompletionTest (test_waspCompletion) where

import qualified Data.Text as T
import E2E
import GHC.Stack (HasCallStack)

test_waspCompletion :: TestTree
test_waspCompletion =
  testGroup
    "wasp-completion"
    [ -- Ideally we would test this without calling `wasp cli completion:list`
      -- but I didn't find a nice way to do it so far.
      test "complete-partial-word" $ assertCompletion "wasp-cli tele" "telemetry",
      test "complete-full-word" $ assertCompletion "wasp-cli telemetry" "telemetry",
      test "complete-multiple-choice" $ assertCompletion "wasp-cli d" "doctor\ndb\ndeploy\ndeps\ndockerfile",
      -- A completion that does nothing would also print nothing, so we first
      -- check that it works.
      test "complete-unknown-empty" $ do
        assertCompletion "wasp-cli tele" "telemetry"
        assertCompletion "wasp-cli unknown" ""
    ]
  where
    -- Fails where it's called, so the failure says which query it's for.
    assertCompletion :: (HasCallStack) => String -> String -> TestM ()
    assertCompletion query expectedCompletion = do
      result <- run $ withEnv [("COMP_LINE", query)] $ waspCli ["completion:list"]
      T.unpack (T.dropWhileEnd (== '\n') result.stdout) `shouldBe` expectedCompletion
