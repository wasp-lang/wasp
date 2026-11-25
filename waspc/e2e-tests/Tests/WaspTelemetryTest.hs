module Tests.WaspTelemetryTest (test_waspTelemetry) where

import E2E

test_waspTelemetry :: TestTree
test_waspTelemetry =
  testGroup
    "wasp-telemetry"
    [ test "enabled-by-default" $
        run (withoutEnv ["WASP_TELEMETRY_DISABLE"] $ waspCli ["telemetry"])
          >>= assertStdoutContains "Telemetry is currently: ENABLED",
      test "disabled-with-env-var" $
        run (withEnv [("WASP_TELEMETRY_DISABLE", "1")] $ waspCli ["telemetry"])
          >>= assertStdoutContains "Telemetry is currently: DISABLED"
    ]
