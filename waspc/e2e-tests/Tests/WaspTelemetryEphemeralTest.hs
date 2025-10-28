module Tests.WaspTelemetryContainerTest (waspTelemetryEphemeralTest) where

import ShellCommands (waspCliTelemetry, writeToStdErrOnFailureAndExit, (~|))
import EphemeralTest (EphemeralTest, makeEphemeralTest)

waspTelemetryEphemeralTest :: EphemeralTest
waspTelemetryEphemeralTest =
  makeEphemeralTest
    "wasp-telemetry"
    [ -- unsets the `WASP_TELEMETRY_DISABLE` to force the default behavior
      writeToStdErrOnFailureAndExit
        ((~| "grep -q 'ENABLED'") . ("env -u WASP_TELEMETRY_DISABLE " ++)  <$> waspCliTelemetry)
        "Wasp Telemetry should be enabled by default",
      writeToStdErrOnFailureAndExit
        ((~| "grep -q 'DISABLED'") . ("WASP_TELEMETRY_DISABLE=1 " ++) <$> waspCliTelemetry)
        "Wasp Telemetry should be disabled when WASP_TELEMETRY_DISABLE=1 env var is set"
    ]
