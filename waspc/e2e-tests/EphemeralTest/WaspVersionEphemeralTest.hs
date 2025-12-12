module EphemeralTest.WaspVersionEphemeralTest (waspVersionEphemeralTest) where

import EphemeralTest (EphemeralTest, makeEphemeralTest, makeEphemeralTestCase)
import ShellCommands (waspCliVersion, (~|))
import Wasp.Version (waspVersion)

waspVersionEphemeralTest :: EphemeralTest
waspVersionEphemeralTest =
  makeEphemeralTest
    "wasp-version"
    [ makeEphemeralTestCase
        "Should match the `waspc` project Wasp version"
        ((~| ("{ read ver; [ \"$ver\" = '" ++ show waspVersion ++ "' ]; }")) . (~| "head -n1") <$> waspCliVersion)
    ]
