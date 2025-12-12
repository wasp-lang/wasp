module EphemeralTest.WaspCompletionEphemeralTest (waspCompletionEphemeralTest) where

import EphemeralTest (EphemeralTest, makeEphemeralTest, makeEphemeralTestCase)
import ShellCommands (ShellCommand, ShellCommandBuilder, (~&&))

waspCompletionEphemeralTest :: EphemeralTest
waspCompletionEphemeralTest =
  makeEphemeralTest
    "wasp-completion"
    [ -- Ideally we would test this without calling `wasp cli completion:list`
      -- but I didn't find a nice way to do it so far.
      makeEphemeralTestCase
        "Should complete part of word: 'wasp tele' ~> 'telemetry'"
        (assertWaspCliCompletion "wasp-cli tele" "telemetry"),
      makeEphemeralTestCase
        "Should complete full word: 'wasp telemetry' ~> 'telemetry'"
        (assertWaspCliCompletion "wasp-cli telemetry" "telemetry"),
      makeEphemeralTestCase
        "Should complete multiple choice: 'wasp d' ~> 'db' | 'deploy' | 'deps' | 'dockerfile'"
        (assertWaspCliCompletion "wasp-cli d" "db\ndeploy\ndeps\ndockerfile"),
      makeEphemeralTestCase
        "Should reutrn empty string for unknown completion: 'wasp unknown' ~> ''"
        (assertWaspCliCompletion "wasp-cli unknown" "")
    ]
  where
    assertWaspCliCompletion :: String -> String -> ShellCommandBuilder context ShellCommand
    assertWaspCliCompletion query expectedCompletion =
      return $
        "export COMP_LINE='"
          ++ query
          ++ "'"
            ~&& "[ \"$(wasp-cli completion:list)\" = \""
          ++ expectedCompletion
          ++ "\" ]"
