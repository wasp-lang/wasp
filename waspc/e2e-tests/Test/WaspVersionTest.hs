module Test.WaspVersionTest (waspVersionTest) where

import ShellCommands (waspCliVersion, (~|))
import Test (Test, makeTest, makeTestCase)
import Wasp.Version (waspVersion)

waspVersionTest :: Test
waspVersionTest =
  makeTest
    "wasp-version"
    [ makeTestCase
        "Should match the `waspc` project Wasp version"
        ((~| ("{ read ver; [ \"$ver\" = '" ++ show waspVersion ++ "' ]; }")) . (~| "head -n1") <$> waspCliVersion)
    ]
