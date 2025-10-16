module Tests.WaspCompletionEphemeralTest (waspCompletionEphemeralTest) where

import EphemeralTest (EphemeralTest, makeEphemeralTest)
import ShellCommands (writeToStdErrOnFailureAndExit, (~&&), ShellCommand, ShellCommandBuilder)

waspCompletionEphemeralTest :: EphemeralTest
waspCompletionEphemeralTest =
  makeEphemeralTest
    "wasp-completion"
    [ -- Ideally we would test this without calling `wasp cli completion:list`
      -- but I didn't find a nice way to do it so far.
      writeToStdErrOnFailureAndExit
        (assertWaspCliCompletion "wasp-cli tele" "telemetry")
        "Didn't complete part of word: 'wasp tele' ~> 'telemetry'",
      writeToStdErrOnFailureAndExit
        (assertWaspCliCompletion "wasp-cli telemetry" "telemetry")
        "Didn't complete full word: 'wasp telemetry' ~> 'telemetry'",
      writeToStdErrOnFailureAndExit
        (assertWaspCliCompletion "wasp-cli d" "db\ndeploy\ndeps\ndockerfile")
        "Didn't complete multiple choice: 'wasp d' ~> 'db' | 'deploy' | 'deps' | 'dockerfile'",
      writeToStdErrOnFailureAndExit
        (assertWaspCliCompletion "wasp-cli unknown" "")
        "Didn't complete empty string for uknown command: 'wasp unknown' ~> ''"
    ]
  where
    -- Note Wasp completion only uses COMP_LINE, but it's correct to also set the COMP_POINT.
    assertWaspCliCompletion :: String -> String -> ShellCommandBuilder context ShellCommand
    assertWaspCliCompletion query expectedCompletion =
      return $
        "export COMP_LINE='" ++ query ++ "'"
          ~&& "export COMP_POINT='" ++ show (length query) ++ "'"
          ~&& "[ \"$(wasp-cli completion:list)\" = \"" ++ expectedCompletion ++ "\" ]"
