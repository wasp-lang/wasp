module EphemeralTest.WaspVersionEphemeralTest (waspVersionEphemeralTest) where

import EphemeralTest (EphemeralTest, makeEphemeralTest)
import ShellCommands (waspCliVersion, writeToStdErrOnFailureAndExit, (~|))
import Wasp.Version (waspVersion)

waspVersionEphemeralTest :: EphemeralTest
waspVersionEphemeralTest =
  makeEphemeralTest
    "wasp-version"
    [ writeToStdErrOnFailureAndExit
        ((~| ("{ read ver; [ \"$ver\" = '" ++ show waspVersion ++ "' ]; }")) . (~| "head -n1") <$> waspCliVersion)
        "Wasp CLI version should match the `waspc` project Wasp version"
    ]
