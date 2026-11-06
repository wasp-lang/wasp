module Tests.WaspVersionTest (waspVersionTest) where

import qualified Data.Text as T
import E2E
import Wasp.Version (waspVersion)

waspVersionTest :: E2ETest
waspVersionTest =
  testGroup
    "wasp-version"
    [ test "match-waspc-version" $ do
        result <- wasp ["version"]
        assertEqual "first line of `wasp version`" (show waspVersion) (T.unpack . T.strip . T.takeWhile (/= '\n') $ result.stdout)
    ]
