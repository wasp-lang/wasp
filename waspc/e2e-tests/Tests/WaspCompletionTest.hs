module Tests.WaspCompletionTest (waspCompletionTest) where

import qualified Data.Text as T
import E2E

waspCompletionTest :: E2ETest
waspCompletionTest =
  testGroup
    "wasp-completion"
    [ -- Ideally we would test this without calling `wasp cli completion:list`
      -- but I didn't find a nice way to do it so far.
      test "complete-partial-word" $ assertCompletion "wasp-cli tele" "telemetry",
      test "complete-full-word" $ assertCompletion "wasp-cli telemetry" "telemetry",
      test "complete-multiple-choice" $ assertCompletion "wasp-cli d" "doctor\ndb\ndeploy\ndeps\ndockerfile",
      test "complete-unknown-empty" $ assertCompletion "wasp-cli unknown" ""
    ]
  where
    assertCompletion :: String -> String -> TestM ()
    assertCompletion query expectedCompletion = do
      result <- run $ withEnv [("COMP_LINE", query)] $ waspCli ["completion:list"]
      assertEqual ("completion of " ++ show query) expectedCompletion (T.unpack . T.dropWhileEnd (== '\n') $ result.stdout)
