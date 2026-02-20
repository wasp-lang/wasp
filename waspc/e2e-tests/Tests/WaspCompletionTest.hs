module Tests.WaspCompletionTest (waspCompletionTest) where

import ShellCommands (ShellCommand, (~&&))
import Test (Test (..), TestCase (..))

waspCompletionTest :: Test
waspCompletionTest =
  Test
    "wasp-completion"
    [ -- Ideally we would test this without calling `wasp cli completion:list`
      -- but I didn't find a nice way to do it so far.
      TestCase
        "complete-partial-word"
        (return [assertWaspCliCompletion "wasp-cli tele" "telemetry"]),
      TestCase
        "complete-full-word"
        (return [assertWaspCliCompletion "wasp-cli telemetry" "telemetry"]),
      TestCase
        "complete-multiple-choice"
        (return [assertWaspCliCompletion "wasp-cli d" "db\ndeploy\ndeps\ndockerfile"]),
      TestCase
        "complete-unknown-empty"
        (return [assertWaspCliCompletion "wasp-cli unknown" ""])
    ]
  where
    assertWaspCliCompletion :: String -> String -> ShellCommand
    assertWaspCliCompletion query expectedCompletion =
      ("export COMP_LINE='" ++ query ++ "'")
        ~&& ("[ \"$(wasp-cli completion:list)\" = \"" ++ expectedCompletion ++ "\" ]")
