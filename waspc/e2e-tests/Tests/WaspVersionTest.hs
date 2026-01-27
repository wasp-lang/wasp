module Tests.WaspVersionTest (waspVersionTest) where

import ShellCommands (ShellCommand, (~|))
import Test (Test, makeTest, makeTestCase)
import Wasp.Version (waspVersion)

waspVersionTest :: Test
waspVersionTest =
  makeTest
    "wasp-version"
    [ makeTestCase
        "match-waspc-version"
        (return . (: []) $ (~| ("{ read ver; [ \"$ver\" = '" ++ show waspVersion ++ "' ]; }")) . (~| "head -n1") $ waspCliVersion)
    ]

waspCliVersion :: ShellCommand
waspCliVersion = "wasp-cli version"
