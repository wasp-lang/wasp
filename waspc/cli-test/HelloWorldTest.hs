module HelloWorldTest where

import Test.Tasty.Hspec

spec_helloWorld :: Spec
spec_helloWorld = do
  it "runs, yay!" $ do
    "Hello, world!" `shouldEndWith` "world!"
