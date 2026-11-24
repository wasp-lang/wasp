module Tests.WaspVersionTest (waspVersionTest) where

import E2E
import Wasp.Version (waspVersion)

waspVersionTest :: E2ETest
waspVersionTest =
  testGroup
    "wasp-version"
    [ test "match-waspc-version" $ do
        firstLine <- wasp ["version"] >>= firstStdoutLine
        assertEqual "first line of `wasp version`" (show waspVersion) firstLine
    ]
