module Tests.WaspCompletionTest (waspCompletionTests) where

import qualified Data.Text as T
import E2e

waspCompletionTests :: [E2eTest]
waspCompletionTests =
  testGroup
    "wasp-completion"
    -- Ideally we would test this without calling `wasp completion:list`,
    -- but we haven't found a nice way to do it so far.
    [ test "complete-partial-word" $ assertCompletion "wasp-cli tele" "telemetry",
      test "complete-full-word" $ assertCompletion "wasp-cli telemetry" "telemetry",
      test "complete-multiple-choice" $ assertCompletion "wasp-cli d" "doctor\ndb\ndeploy\ndeps\ndockerfile",
      test "complete-unknown-empty" $ assertCompletion "wasp-cli unknown" ""
    ]
  where
    assertCompletion :: String -> T.Text -> TestM ()
    assertCompletion commandLine expectedCompletion = do
      result <- run $ withEnv [("COMP_LINE", commandLine)] $ waspCli ["completion:list"]
      assertEqual ("completions for " ++ show commandLine) expectedCompletion (T.stripEnd result.stdout)
