module Tests.WaspVersionTest (waspVersionTest) where

import ShellCommands (ShellCommand, (~|))
import Test (Test (..), TestCase (..))
import Wasp.Version (waspVersion)

waspVersionTest :: Test
waspVersionTest =
  Test
    "wasp-version"
    [ TestCase
        "match-waspc-version"
        (return . (: []) $ (~| ("{ read ver; [ \"$ver\" = '" ++ show waspVersion ++ "' ]; }")) . (~| "head -n1") $ waspCliVersion)
    ]

waspCliVersion :: ShellCommand
waspCliVersion = "wasp-cli version"
