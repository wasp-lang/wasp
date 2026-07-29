module Tests.WaspCompletionTest (waspCompletionTest) where

import Command (withEnvVars)
import Context (TestContext)
import SharedActions (assertCommandStdoutTrimmedEquals, waspCli)
import Test (Test (..), TestCase (..))
import TestAction (TestAction)

waspCompletionTest :: Test
waspCompletionTest =
  Test
    "wasp-completion"
    [ -- Ideally we would test this without calling `wasp cli completion:list`
      -- but I didn't find a nice way to do it so far.
      TestCase "complete-partial-word" $
        assertWaspCliCompletion "wasp-cli tele" "telemetry",
      TestCase "complete-full-word" $
        assertWaspCliCompletion "wasp-cli telemetry" "telemetry",
      TestCase "complete-multiple-choice" $
        assertWaspCliCompletion "wasp-cli d" "doctor\ndb\ndeploy\ndeps\ndockerfile",
      TestCase "complete-unknown-empty" $
        assertWaspCliCompletion "wasp-cli unknown" ""
    ]
  where
    assertWaspCliCompletion :: String -> String -> TestAction TestContext ()
    assertWaspCliCompletion query expectedCompletion =
      assertCommandStdoutTrimmedEquals
        (withEnvVars [("COMP_LINE", query)] (waspCli ["completion:list"]))
        expectedCompletion
