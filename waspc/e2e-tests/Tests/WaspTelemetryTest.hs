module Tests.WaspTelemetryTest (waspTelemetryTest) where

import E2E

waspTelemetryTest :: E2ETest
waspTelemetryTest =
  testGroup
    "wasp-telemetry"
    [ test "enabled-by-default" $
        run (withoutEnv ["WASP_TELEMETRY_DISABLE"] $ waspCli ["telemetry"]) >>= assertOutputContains "ENABLED",
      test "disabled-with-env-var" $
        run (withEnv [("WASP_TELEMETRY_DISABLE", "1")] $ waspCli ["telemetry"]) >>= assertOutputContains "DISABLED"
    ]
