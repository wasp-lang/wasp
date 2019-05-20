module UtilTest where

import Test.Tasty.Hspec

import Util


spec_camelToKebabCase :: Spec
spec_camelToKebabCase = do
    "foobar" ~> "foobar"
    "s3" ~> "s3"
    "fooBarBar" ~> "foo-bar-bar"
    "s3Folder" ~> "s3-folder"
    "S3Folder" ~> "s3-folder"
  where
    camel ~> kebab = it (camel ++ " -> " ++ kebab) $ do
        camelToKebabCase camel `shouldBe` kebab

spec_onFirst :: Spec
spec_onFirst = do
    it "Returns empty list for empty list" $ do
        (onFirst id ([] :: [Char])) `shouldBe` []
    it "Applies given method on first element of list" $ do
        onFirst (+ 1) ([1, 2, 3] :: [Int]) `shouldBe` [2, 2, 3]

spec_toLowerFirst :: Spec
spec_toLowerFirst = do
    it "Lowers first letter of string" $ do
        toLowerFirst "FooBar" `shouldBe` "fooBar"

spec_toUpperFirst :: Spec
spec_toUpperFirst = do
    it "Capitalizes first letter of string" $ do
        toUpperFirst "fooBar" `shouldBe` "FooBar"
