import qualified Test.Tasty
import Test.Tasty.Hspec
import Test.Tasty.QuickCheck as QC

import Lib

main :: IO ()
main = do
  test <- testSpec "stic-test" spec
  Test.Tasty.defaultMain test

spec :: Spec
spec = parallel $ do
  it "fibonacci element #0 is 0" $ do
    fibonacci 0 `shouldBe` 0
  it "fibonacci element #1 is 1" $ do
    fibonacci 1 `shouldBe` 1
  it "fibonacci element #2 is 1" $ do
    fibonacci 2 `shouldBe` 1
  it "fibonacci element #3 is 2" $ do
    fibonacci 3 `shouldBe` 2

-- TODO: Figure out how to organize tests for multiple modules!
--   I should read https://documentup.com/feuerbach/tasty, especially "Project organization and integration with Cabal"
--   part, and then continue from there. Search for some examples, maybe existing projects.
--   Also, can tests be mixed with source files as we usually do it in other languages? They certainly can,
--   but is that better and if yes how would be implement that?
--   Do we need smth like tasty-discovery?
