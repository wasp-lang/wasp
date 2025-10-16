module Tests.WaspCompletionContainerTest (waspCompletionContainerTest) where

import ContainerTest (ContainerTest, makeContainerTest)
import ShellCommands ((~&&))

-- TODO: we don't actually need to run this inside of container
-- move to emphereal tests when added
waspCompletionContainerTest :: ContainerTest
waspCompletionContainerTest =
  makeContainerTest
    "wasp-completion"
    [ -- Ideally we would test this without calling `wasp cli completion:list` directly
      -- but I didn't find a nice way to do it so far.
      testWaspCliCompletionCommand "wasp-cli tele" "telemetry",
      testWaspCliCompletionCommand "wasp-cli telemetry" "telemetry",
      testWaspCliCompletionCommand "wasp-cli dockerf" "dockerfile",
      testWaspCliCompletionCommand "wasp-cli d" "db\ndeploy\ndeps\ndockerfile",
      testWaspCliCompletionCommand "wasp-cli unknown" ""
    ]
  where
    -- Note Wasp completion only uses COMP_LINE, but it's correct to also set the COMP_POINT.
    testWaspCliCompletionCommand query expectedCompletion =
      return $
        "export COMP_LINE='" ++ query ++ "'"
          ~&& "export COMP_POINT='" ++ show (length query) ++ "'"
          ~&& "[[ \"$(wasp-cli completion:list)\" == '" ++ expectedCompletion ++ "' ]]"
