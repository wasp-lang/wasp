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
        "Should complete part of word: 'wasp tele' ~> 'telemetry'"
        (return [assertWaspCliCompletion "wasp-cli tele" "telemetry"]),
      makeTestCase
        "Should complete full word: 'wasp telemetry' ~> 'telemetry'"
        (return [assertWaspCliCompletion "wasp-cli telemetry" "telemetry"]),
      makeTestCase
        "Should complete multiple choice: 'wasp d' ~> 'db' | 'deploy' | 'deps' | 'dockerfile'"
        (return [assertWaspCliCompletion "wasp-cli d" "db\ndeploy\ndeps\ndockerfile"]),
      makeTestCase
        "Should reutrn empty string for unknown completion: 'wasp unknown' ~> ''"
        (return [assertWaspCliCompletion "wasp-cli unknown" ""])
    ]
  where
    assertWaspCliCompletion :: String -> String -> ShellCommand
    assertWaspCliCompletion query expectedCompletion =
      ("export COMP_LINE='" ++ query ++ "'")
        ~&& ("[ \"$(wasp-cli completion:list)\" = \"" ++ expectedCompletion ++ "\" ]")
