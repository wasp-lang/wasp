module Path.ExtraTest where

import Path (reldir)
import qualified Path.Extra as PE
import Test.Tasty.Hspec

spec_reversePosixPath :: Spec
spec_reversePosixPath = do
  [reldir|.|] ~> "."
  [reldir|foo|] ~> ".."
  [reldir|foo/bar|] ~> "../.."
  [reldir|./foo/bar/./test|] ~> "../../.."
  where
    path ~> expectedReversedPath = it ((show path) ++ " -> " ++ expectedReversedPath) $ do
      PE.reversePosixPath (PE.toPosixFilePath path) `shouldBe` expectedReversedPath
