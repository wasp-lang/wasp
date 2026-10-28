module Tests.WaspTelemetryTest (waspTelemetryTests) where

import E2e

waspTelemetryTests :: [E2eTest]
waspTelemetryTests =
  testGroup
    "wasp-telemetry"
    [ test "enabled-by-default" $
        run (withoutEnv ["WASP_TELEMETRY_DISABLE"] $ waspCli ["telemetry"])
          >>= assertOutputContains "Telemetry is currently: ENABLED",
      test "disabled-with-env-var" $
        run (withEnv [("WASP_TELEMETRY_DISABLE", "1")] $ waspCli ["telemetry"])
          >>= assertOutputContains "Telemetry is currently: DISABLED"
    ]
