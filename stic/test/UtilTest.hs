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
