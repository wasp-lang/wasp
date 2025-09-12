module FilePath.ExtraTest where

import qualified FilePath.Extra as PE
import StrongPath (reldirP, toFilePath)
import Test.Tasty.Hspec

spec_reversePosixPath :: Spec
spec_reversePosixPath = do
  [reldirP|.|] ~> "."
  [reldirP|foo|] ~> ".."
  [reldirP|foo/bar|] ~> "../.."
  [reldirP|./foo/bar/./test|] ~> "../../.."
  where
    path ~> expectedReversedPath = it (show path ++ " -> " ++ expectedReversedPath) $ do
      PE.reversePosixPath (toFilePath path) `shouldBe` expectedReversedPath
