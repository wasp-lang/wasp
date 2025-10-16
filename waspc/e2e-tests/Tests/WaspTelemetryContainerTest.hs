module Tests.WaspTelemetryContainerTest (waspTelemetryContainerTest) where

import ContainerTest (ContainerTest, makeContainerTest)
import ShellCommands (waspCliTelemetry, (~|))

-- TODO: we don't actually need to run this inside of container
-- move to emphereal tests when added
waspTelemetryContainerTest :: ContainerTest
waspTelemetryContainerTest =
  makeContainerTest
    "wasp-telemetry"
    [ (~| "grep -q 'ENABLED'") <$> waspCliTelemetry,
      (~| "grep -q 'DISABLED'") . ("WASP_TELEMETRY_DISABLE=1 " ++) <$> waspCliTelemetry
    ]
