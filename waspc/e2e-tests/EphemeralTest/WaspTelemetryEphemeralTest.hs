module EphemeralTest.WaspTelemetryEphemeralTest (waspTelemetryEphemeralTest) where

import EphemeralTest (EphemeralTest, makeEphemeralTest, makeEphemeralTestCase)
import ShellCommands (waspCliTelemetry, (~|))

waspTelemetryEphemeralTest :: EphemeralTest
waspTelemetryEphemeralTest =
  makeEphemeralTest
    "wasp-telemetry"
    [ makeEphemeralTestCase
        "`Should be enabled by default"
        (("env -u WASP_TELEMETRY_DISABLE " ++) . (~| "grep -q 'ENABLED'") <$> waspCliTelemetry),
      makeEphemeralTestCase
        "`Should be disabled when WASP_TELEMETRY_DISABLE=1 env var is set"
        (("WASP_TELEMETRY_DISABLE=1 " ++) . (~| "grep -q 'DISABLED'") <$> waspCliTelemetry)
    ]
