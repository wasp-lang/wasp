module Tests.WaspVersionTest (waspVersionTest) where

import Steps (assertCommandStdoutFirstLineEquals, waspCliVersion)
import Test (Test (..), TestCase (..))
import Wasp.Version (waspVersion)

waspVersionTest :: Test
waspVersionTest =
  Test
    "wasp-version"
    [ TestCase
        "match-waspc-version"
        (sequence [assertCommandStdoutFirstLineEquals waspCliVersion (show waspVersion)])
    ]
