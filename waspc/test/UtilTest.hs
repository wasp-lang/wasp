module UtilTest where

import Control.DeepSeq
import Control.Exception (evaluate)
import Data.Aeson (object, toJSON, (.=))
import qualified Data.Aeson as Aeson
import Test.Tasty.Hspec
import Wasp.Util

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
    onFirst id ([] :: [Char]) `shouldBe` []
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

spec_jsonSet :: Spec
spec_jsonSet = do
  let inputObj =
        object
          [ "prop1" .= ("first" :: String)
          ]

  it "When input JSON is not an object, throws an error." $ do
    (evaluate . force) (jsonSet "someProp" (Aeson.Number 23) (Aeson.Bool True))
      `shouldThrow` errorCall "Input JSON must be an object"

  it "When a new property is set, result object contains it along with the original ones." $ do
    let expectedObj =
          object
            [ "prop1" .= ("first" :: String),
              "newProp" .= (23 :: Int)
            ]
    jsonSet "newProp" (Aeson.Number 23) inputObj `shouldBe` expectedObj

  it "When an existing property is set, it is overwritten in the result object." $ do
    let newStrValue = "newVal" :: String
    let expectedObj =
          object
            [ "prop1" .= newStrValue
            ]
    jsonSet "prop1" (toJSON newStrValue) inputObj `shouldBe` expectedObj
