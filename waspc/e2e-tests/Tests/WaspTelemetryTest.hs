module Tests.WaspTelemetryTest (waspTelemetryTest) where

import Command (withEnvVars, withoutEnvVars)
import Steps (assertCommandSucceedsWithOutputContaining, waspCliTelemetry)
import Test (Test (..), TestCase (..))

waspTelemetryTest :: Test
waspTelemetryTest =
  Test
    "wasp-telemetry"
    [ TestCase
        "enabled-by-default"
        [ assertCommandSucceedsWithOutputContaining
            (withoutEnvVars ["WASP_TELEMETRY_DISABLE"] waspCliTelemetry)
            "ENABLED"
        ],
      TestCase
        "disabled-with-env-var"
        [ assertCommandSucceedsWithOutputContaining
            (withEnvVars [("WASP_TELEMETRY_DISABLE", "1")] waspCliTelemetry)
            "DISABLED"
        ]
    ]
