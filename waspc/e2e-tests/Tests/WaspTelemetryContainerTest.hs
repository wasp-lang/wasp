module Tests.WaspTelemetryContainerTest (waspTelemetryContainerTest) where

import ContainerTest (ContainerTest, makeContainerTest)
import ShellCommands (waspCliTelemetry, writeToStdErrOnFailureAndExit, (~|))

waspTelemetryContainerTest :: ContainerTest
waspTelemetryContainerTest =
  makeContainerTest
    "wasp-telemetry"
    [ writeToStdErrOnFailureAndExit
        ((~| "grep -q 'ENABLED'") <$> waspCliTelemetry)
        "Wasp Telemetry is disabled by default when it shouldn't be",
      writeToStdErrOnFailureAndExit
        ((~| "grep -q 'DISABLED'") . ("WASP_TELEMETRY_DISABLE=1 " ++) <$> waspCliTelemetry)
        "Wasp Telemetry is enabled even though we set the WASP_TELEMETRY_DISABLE=1 env var"
    ]
