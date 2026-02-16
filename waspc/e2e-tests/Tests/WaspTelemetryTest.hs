module Tests.WaspTelemetryTest (waspTelemetryTest) where

import ShellCommands (ShellCommand, (~|))
import Test (Test (..), TestCase (..))

waspTelemetryTest :: Test
waspTelemetryTest =
  Test
    "wasp-telemetry"
    [ TestCase
        "enabled-by-default"
        (return . (: []) $ ("env -u WASP_TELEMETRY_DISABLE " ++) . (~| "grep -q 'ENABLED'") $ waspCliTelemetry),
      TestCase
        "disabled-with-env-var"
        (return . (: []) $ ("WASP_TELEMETRY_DISABLE=1 " ++) . (~| "grep -q 'DISABLED'") $ waspCliTelemetry)
    ]

waspCliTelemetry :: ShellCommand
waspCliTelemetry = "wasp-cli telemetry"
