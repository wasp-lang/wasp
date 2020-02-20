module Path.ExtraTest where

import Test.Tasty.Hspec

import Path (reldir)
import qualified Path.Extra as PE


spec_reversePath :: Spec
spec_reversePath = do
    [reldir|.|] ~> "."
    [reldir|foo|] ~> ".."
    [reldir|foo/bar|] ~> "../.."
    [reldir|./foo/bar/./test|] ~> "../../.."
  where
    path ~> expectedReversedPath = it ((show path) ++ " -> " ++ expectedReversedPath) $ do
        PE.reversePath path `shouldBe` expectedReversedPath
