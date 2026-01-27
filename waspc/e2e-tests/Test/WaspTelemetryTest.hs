module Test.WaspTelemetryTest (waspTelemetryTest) where

import ShellCommands (ShellCommand, (~|))
import Test (Test, makeTest, makeTestCase)

waspTelemetryTest :: Test
waspTelemetryTest =
  makeTest
    "wasp-telemetry"
    [ makeTestCase
        "Should be enabled by default"
        (return . (: []) $ ("env -u WASP_TELEMETRY_DISABLE " ++) . (~| "grep -q 'ENABLED'") $ waspCliTelemetry),
      makeTestCase
        "Should be disabled when WASP_TELEMETRY_DISABLE=1 env var is set"
        (return . (: []) $ ("WASP_TELEMETRY_DISABLE=1 " ++) . (~| "grep -q 'DISABLED'") $ waspCliTelemetry)
    ]

waspCliTelemetry :: ShellCommand
waspCliTelemetry = "wasp-cli telemetry"
