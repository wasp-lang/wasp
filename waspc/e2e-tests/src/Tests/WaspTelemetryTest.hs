module Tests.WaspTelemetryTest (test_waspTelemetry) where

import E2E

test_waspTelemetry :: TestTree
test_waspTelemetry =
  testGroup
    "wasp-telemetry"
    [ test "enabled-by-default" $
        run (withoutEnv [telemetryDisableEnvVarName] $ waspCli ["telemetry"])
          >>= assertStdoutContains "Telemetry is currently: ENABLED",
      test "disabled-with-env-var" $
        run (withEnv [(telemetryDisableEnvVarName, "1")] $ waspCli ["telemetry"])
          >>= assertStdoutContains "Telemetry is currently: DISABLED"
    ]
  where
    telemetryDisableEnvVarName = "WASP_TELEMETRY_DISABLE"
