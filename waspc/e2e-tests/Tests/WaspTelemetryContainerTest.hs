module Tests.WaspTelemetryContainerTest (waspTelemetryContainerTest) where

import ContainerTest (ContainerTest, makeContainerTest)
import ShellCommands (waspCliTelemetry, (~|), appendToFile)

waspTelemetryContainerTest :: ContainerTest
waspTelemetryContainerTest =
  makeContainerTest
    "wasp-telemetry"
    [
       (~| "grep -q 'ENABLED'") <$> waspCliTelemetry,
      appendToFile "~/.profile" "WASP_TELEMETRY_DISABLE=1",
       (~| "grep -q 'DISABLED'") <$> waspCliTelemetry
    ]
