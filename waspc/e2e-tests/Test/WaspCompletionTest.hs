module Test.WaspCompletionTest (waspCompletionTest) where

import ShellCommands (ShellCommand, (~&&))
import Test (Test, makeTest, makeTestCase)

waspCompletionTest :: Test
waspCompletionTest =
  makeTest
    "wasp-completion"
    [ -- Ideally we would test this without calling `wasp cli completion:list`
      -- but I didn't find a nice way to do it so far.
      makeTestCase
        "complete-partial-word"
        (return [assertWaspCliCompletion "wasp-cli tele" "telemetry"]),
      makeTestCase
        "complete-full-word"
        (return [assertWaspCliCompletion "wasp-cli telemetry" "telemetry"]),
      makeTestCase
        "complete-multiple-choice"
        (return [assertWaspCliCompletion "wasp-cli d" "db\ndeploy\ndeps\ndockerfile"]),
      makeTestCase
        "complete-unknown-empty"
        (return [assertWaspCliCompletion "wasp-cli unknown" ""])
    ]
  where
    assertWaspCliCompletion :: String -> String -> ShellCommand
    assertWaspCliCompletion query expectedCompletion =
      ("export COMP_LINE='" ++ query ++ "'")
        ~&& ("[ \"$(wasp-cli completion:list)\" = \"" ++ expectedCompletion ++ "\" ]")
