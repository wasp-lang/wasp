module UtilTest where

import Control.DeepSeq
import Control.Exception (evaluate)
import Data.Aeson (object, toJSON, (.=))
import qualified Data.Aeson as Aeson
import qualified Data.Map as Map
import Test.Hspec
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

spec_kebabToCamelCase :: Spec
spec_kebabToCamelCase = do
  "foobar" ~> "foobar"
  "s3" ~> "s3"
  "foo-bar-bar" ~> "fooBarBar"
  "todo-App" ~> "todoApp"
  "TODO-app" ~> "TODOApp"
  "s3-folder" ~> "s3Folder"
  "-s3--folder---" ~> "s3Folder"
  "foo---bar-baz" ~> "fooBarBaz"
  "-foo-" ~> "foo"
  "-" ~> ""
  "--" ~> ""
  "" ~> ""
  where
    kebab ~> camel = it (kebab ++ " -> " ++ camel) $ do
      kebabToCamelCase kebab `shouldBe` camel

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

spec_second3 :: Spec
spec_second3 = do
  it "Applies the function to the second element of a tuple of size 3." $ do
    second3 negate ("1" :: String, 2 :: Int, True) `shouldBe` ("1", -2, True)

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
  describe "Should indent the given text correctly" $ do
    it "When given only a single line of text" $ do
      indent 2 "foo" `shouldBe` "  foo"
    it "When given multiple lines of text" $ do
      indent 3 "foo\nbar" `shouldBe` "   foo\n   bar"
    it "When given a single line of text with existing indentation" $ do
      indent 4 "  foo" `shouldBe` "      foo"
    it "When given multiple lines of text with existing indentation" $ do
      indent 4 "foo\n  bar" `shouldBe` "    foo\n      bar"
    it "When given text containing empty lines" $ do
      indent 4 "foo\n\nbar\n  baz\n\n  ban" `shouldBe` "    foo\n\n    bar\n      baz\n\n      ban"

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

spec_wrapString :: Spec
spec_wrapString = do
  describe "wrapString" $ do
    it "returns an empty string for empty input" $ do
      wrapString 10 "" `shouldBe` ""

    it "returns a single line when text fits" $ do
      wrapString 20 "hello world" `shouldBe` "hello world"

    it "wraps a string that exceeds max length" $ do
      wrapString 10 "hello world" `shouldBe` "hello\nworld"

    it "never breaks long words even if they exceed max length" $ do
      wrapString 5 "extraordinary day is extraordinary" `shouldBe` "extraordinary\nday\nis\nextraordinary"

    it "wraps text with many words into multiple lines" $ do
      wrapString 20 "one two three four five six seven eight nine ten"
        `shouldBe` "one two three four\nfive six seven eight\nnine ten"

    it "treats newlines as spaces" $ do
      wrapString 15 "hello\nworld this\nis a test"
        `shouldBe` "hello world\nthis is a test"

    it "handles multiple consecutive long words" $ do
      wrapString 5 "internationalization globalization"
        `shouldBe` "internationalization\nglobalization"

    it "collapses multiple spaces between words" $ do
      wrapString 20 "hello   world  this   is   spacy"
        `shouldBe` "hello world this is\nspacy"

    it "collapses whitespace-only input to empty string" $ do
      wrapString 10 "   " `shouldBe` ""

    it "returns each word on its own line when maxLength is 0" $ do
      wrapString 0 "one two three"
        `shouldBe` "one\ntwo\nthree"

    it "treats negative maxLength as zero" $ do
      wrapString (-5) "one two" `shouldBe` wrapString 0 "one two"

spec_hex :: Spec
spec_hex = do
  it "Correctly transforms bytestring to hex" $ do
    bytestringToHex "test" `shouldBe` hexFromString "74657374"

spec_checksum :: Spec
spec_checksum = do
  it "Correctly calculates checksum of string" $ do
    checksumFromString "test" `shouldBe` hexFromString "9f86d081884c7d659a2feaa0c55ad015a3bf4f1b2b0b822cd15d6c15b0f00a08"

spec_findDuplicateElems :: Spec
spec_findDuplicateElems = do
  it "Finds duplicate elements in a list" $ do
    findDuplicateElems ([1, 2, 3, 4, 5, 1, 2, 3, 4, 5] :: [Int]) `shouldBe` [1, 2, 3, 4, 5]

  it "Returns empty list if there are no duplicates" $ do
    findDuplicateElems ([1, 2, 3, 4, 5] :: [Int]) `shouldBe` []

  it "Returns empty list for empty list" $ do
    findDuplicateElems ([] :: [Int]) `shouldBe` []

spec_checkIfEnvValueIsTruthy :: Spec
spec_checkIfEnvValueIsTruthy = do
  it "Correctly determines if different env values are truthy" $ do
    let testCases =
          [ (Nothing, False),
            (Just "", False),
            (Just "false", False),
            (Just "False", False),
            (Just "FALSE", False),
            (Just "true", True),
            (Just "something", True),
            (Just "0", True),
            (Just "1", True),
            (Just "falsy", True),
            (Just "foo", True)
          ]
    checkIfEnvValueIsTruthy . fst <$> testCases
      `shouldBe` snd <$> testCases

spec_zipMaps :: Spec
spec_zipMaps = do
  describe "zipMaps" $ do
    let zipMapToTuples :: Map.Map String Int -> Map.Map String Int -> Map.Map String (Maybe Int, Maybe Int)
        zipMapToTuples = zipMaps $ \_ ma mb -> Just (ma, mb)

    it "returns empty map when both input maps are empty" $ do
      let map1 = Map.empty
          map2 = Map.empty
      zipMapToTuples map1 map2
        `shouldBe` Map.empty

    it "processes keys only in first map" $ do
      let map1 = Map.fromList [("a", 1), ("b", 2)]
          map2 = Map.empty
      zipMapToTuples map1 map2
        `shouldBe` Map.fromList
          [ ("a", (Just 1, Nothing)),
            ("b", (Just 2, Nothing))
          ]

    it "processes keys only in second map" $ do
      let map1 = Map.empty
          map2 = Map.fromList [("x", 10), ("y", 20)]
      zipMapToTuples map1 map2
        `shouldBe` Map.fromList
          [ ("x", (Nothing, Just 10)),
            ("y", (Nothing, Just 20))
          ]

    it "processes keys present in both maps" $ do
      let map1 = Map.fromList [("a", 1), ("b", 2)]
          map2 = Map.fromList [("a", 10), ("b", 20)]
      zipMapToTuples map1 map2
        `shouldBe` Map.fromList
          [ ("a", (Just 1, Just 10)),
            ("b", (Just 2, Just 20))
          ]

    it "processes keys from both maps with partial overlap" $ do
      let map1 = Map.fromList [("a", 1), ("b", 2)]
          map2 = Map.fromList [("b", 20), ("c", 30)]
      zipMapToTuples map1 map2
        `shouldBe` Map.fromList
          [ ("a", (Just 1, Nothing)),
            ("b", (Just 2, Just 20)),
            ("c", (Nothing, Just 30))
          ]

    it "filters out keys when function returns Nothing" $ do
      let map1 = Map.fromList [("a", 1), ("b", 2), ("c", 3)]
          map2 = Map.fromList [("a", 10), ("b", 20), ("d", 40)]

          -- Only keep keys present in both maps
          f :: String -> Maybe Int -> Maybe Int -> Maybe Int
          f _ (Just x) (Just y) = Just (x + y)
          f _ _ _ = Nothing

      zipMaps f map1 map2
        `shouldBe` Map.fromList [("a", 11), ("b", 22)]

    it "can use the key in the combining function" $ do
      let map1 = Map.fromList [(1, "a"), (2, "b")]
          map2 = Map.fromList [(2, "c"), (3, "d")]

          f :: Int -> Maybe String -> Maybe String -> Maybe Int
          f k _ _ = Just k

      zipMaps f map1 map2
        `shouldBe` Map.fromList [(1, 1), (2, 2), (3, 3)]

    it "can transform values from different types" $ do
      let map1 = Map.fromList [("x", 5)]
          map2 = Map.fromList [("x", "hello")]

          f :: String -> Maybe Int -> Maybe String -> Maybe String
          f _ (Just n) (Just s) = Just (replicate n (head s))
          f _ _ _ = Nothing

      zipMaps f map1 map2
        `shouldBe` Map.fromList [("x", "hhhhh")]
