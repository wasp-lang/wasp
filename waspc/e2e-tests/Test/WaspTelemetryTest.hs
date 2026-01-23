module Test.WaspTelemetryTest (waspTelemetryTest) where

import ShellCommands (waspCliTelemetry, (~|))
import Test (Test, makeTestCase, makeTest)

waspTelemetryTest :: Test
waspTelemetryTest =
  makeTest
    "wasp-telemetry"
    [ makeTestCase
        "Should be enabled by default"
        (("env -u WASP_TELEMETRY_DISABLE " ++) . (~| "grep -q 'ENABLED'") <$> waspCliTelemetry),
      makeTestCase
        "Should be disabled when WASP_TELEMETRY_DISABLE=1 env var is set"
        (("WASP_TELEMETRY_DISABLE=1 " ++) . (~| "grep -q 'DISABLED'") <$> waspCliTelemetry)
    ]
