module Tests.WaspVersionTest (waspVersionTests) where

import qualified Data.Text as T
import E2e

waspVersionTests :: [E2eTest]
waspVersionTests =
  testGroup
    "wasp-version"
    [ test "match-waspc-version" $ do
        result <- wasp ["version"]
        assertEqual "the first line of `wasp version`" textWaspVersion (firstLine result.stdout)
    ]
  where
    firstLine = T.strip . T.takeWhile (/= '\n')
