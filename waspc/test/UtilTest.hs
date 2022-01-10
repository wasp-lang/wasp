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

spec_indent :: Spec
spec_indent = do
  describe "indent should indent given text correctly" $ do
    it "when just one line of text" $ do
      indent 2 "foo" `shouldBe` "  foo"
    it "when multiple lines of text" $ do
      indent 3 "foo\nbar" `shouldBe` "   foo\n   bar"
    it "when text is already somewhat indented" $ do
      indent 4 "  foo\n  bar" `shouldBe` "      foo\n      bar"

spec_concatShortPrefixAndText :: Spec
spec_concatShortPrefixAndText = do
  describe "concatShortPrefixAndText should" $ do
    it "return prefix if text is empty" $ do
      concatShortPrefixAndText "--" "" `shouldBe` "--"
    it "directly concat if text has single line" $ do
      concatShortPrefixAndText " - " "foo" `shouldBe` " - foo"
    it "align the rest of the lines in text with the first line" $ do
      concatShortPrefixAndText " - " "foo\nbar" `shouldBe` " - foo\n   bar"

spec_concatPrefixAndText :: Spec
spec_concatPrefixAndText = do
  describe "concatPrefixAndText should" $ do
    it "return prefix if text is empty" $ do
      concatPrefixAndText "some prefix: " "" `shouldBe` "some prefix: "
    it "directly concat if text has single line" $ do
      concatPrefixAndText "prefix: " "foo" `shouldBe` "prefix: foo"
    it "put all the text below the prefix, indented for 2 spaces, if text has multiple lines" $ do
      concatPrefixAndText "prefix: " "foo\nbar" `shouldBe` "prefix: \n  foo\n  bar"

spec_leftPad :: Spec
spec_leftPad = do
  describe "leftPad should" $ do
    it "pad the list if it is shorter than desired length" $ do
      leftPad ' ' 5 "hi" `shouldBe` "   hi"
    it "not modify the list if it is already long enough" $ do
      leftPad ' ' 5 "hihih" `shouldBe` "hihih"
      leftPad ' ' 5 "hihihi" `shouldBe` "hihihi"

spec_insertAt :: Spec
spec_insertAt = do
  describe "insertAt should" $ do
    it "insert given list at the start of host list if index is 0 or negative" $ do
      insertAt [0] 0 [1, 2, 3] `shouldBe` ([0, 1, 2, 3] :: [Int])
      insertAt [0] (-1) [1, 2, 3] `shouldBe` ([0, 1, 2, 3] :: [Int])
    it "insert given list in the host list at given index when index is in [1, host list length - 1]" $ do
      insertAt [0] 1 [1, 2, 3] `shouldBe` ([1, 0, 2, 3] :: [Int])
      insertAt [0] 2 [1, 2, 3] `shouldBe` ([1, 2, 0, 3] :: [Int])
    it "insert given list at the end of host list if index is equal or bigger than host list length" $ do
      insertAt [0] 3 [1, 2, 3] `shouldBe` ([1, 2, 3, 0] :: [Int])
      insertAt [0] 4 [1, 2, 3] `shouldBe` ([1, 2, 3, 0] :: [Int])
