module Tests.WaspVersionTest (test_waspVersion) where

import E2E
import Wasp.Version (waspVersion)

test_waspVersion :: TestTree
test_waspVersion =
  testGroup
    "wasp-version"
    [ test "match-waspc-version" $ do
        firstLine <- wasp ["version"] >>= firstStdoutLine
        firstLine `shouldBe` show waspVersion
    ]
