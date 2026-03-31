module SemanticVersion.RangeTest where

import Data.Either (isLeft)
import qualified Data.List.NonEmpty as NE
import Test.Hspec
import qualified Text.Parsec as P
import Wasp.SemanticVersion.PartialVersion
import Wasp.SemanticVersion.Range
import Wasp.SemanticVersion.Version
import Wasp.SemanticVersion.VersionBound

spec_SemanticVersion_Range :: Spec
spec_SemanticVersion_Range = do
  describe "show" $ do
    it "show empty range" $
      show (mempty :: Range) `shouldBe` ""
    it "show simple range" $
      show [r|<=1.3.6|] `shouldBe` "<=1.3.6"
    it "show complex range" $
      show [r|<=1.3.6 ^1.2.0 || 1.2.3|] `shouldBe` "<=1.3.6 ^1.2.0 || 1.2.3"

  describe "parseRange" $ do
    it "parses empty input correctly" $
      parseRange "" `shouldBe` Right (Range [Simple $ pure $ Primitive Equal Any])
    it "parses ranges with single comparator set" $ do
      parseRange "  >=1.0 <2.0.0  "
        `shouldBe` Right
          ( Range
              [ Simple $
                  NE.fromList
                    [ Primitive GreaterThanOrEqual (MajorMinor 1 0),
                      Primitive LessThan (MajorMinorPatch 2 0 0)
                    ]
              ]
          )
      parseRange " ^1.2.3  "
        `shouldBe` Right
          ( Range
              [ Simple $ pure $ CaretRange (MajorMinorPatch 1 2 3)
              ]
          )
    it "parses ranges with multiple comparator sets" $ do
      parseRange "   ^1.2.3 ||   ^2.0 ||"
        `shouldBe` Right
          ( Range
              [ Simple $ pure $ CaretRange (MajorMinorPatch 1 2 3),
                Simple $ pure $ CaretRange (MajorMinor 2 0),
                Simple $ pure $ Primitive Equal Any
              ]
          )
      parseRange "^1.2.3||^2.0     "
        `shouldBe` Right
          ( Range
              [ Simple $ pure $ CaretRange (MajorMinorPatch 1 2 3),
                Simple $ pure $ CaretRange (MajorMinor 2 0)
              ]
          )
      parseRange ">=1  <2|| >=3.0.0    || *  "
        `shouldBe` Right
          ( Range
              [ Simple $
                  NE.fromList
                    [ Primitive GreaterThanOrEqual (Major 1),
                      Primitive LessThan (Major 2)
                    ],
                Simple $ pure $ Primitive GreaterThanOrEqual (MajorMinorPatch 3 0 0),
                Simple $ pure $ Primitive Equal Any
              ]
          )
    it "rejects invalid formats" $ do
      isLeft (parseRange "foo") `shouldBe` True
      isLeft (parseRange "|| 1.23 || 2.0") `shouldBe` True
      isLeft (parseRange "1.23 || $2.0") `shouldBe` True
      isLeft (parseRange "1.23 || 1.0 ||a") `shouldBe` True

  describe "rangeParser" $ do
    let looseParseRange = P.parse rangeParser ""

    it "parses range with trailing content" $ do
      looseParseRange "^1.2.3 || ^2.0 ||a"
        `shouldBe` Right
          ( Range
              [ Simple $ pure $ CaretRange (MajorMinorPatch 1 2 3),
                Simple $ pure $ CaretRange (MajorMinor 2 0)
              ]
          )
      looseParseRange "^1.2.3 || ^2.0 abc"
        `shouldBe` Right
          ( Range
              [ Simple $ pure $ CaretRange (MajorMinorPatch 1 2 3),
                Simple $ pure $ CaretRange (MajorMinor 2 0)
              ]
          )

  it "r quasi quoter" $ do
    [r|^1.2.3 ||   ^2.0|]
      `shouldBe` Range
        [ Simple $ pure $ CaretRange (MajorMinorPatch 1 2 3),
          Simple $ pure $ CaretRange (MajorMinor 2 0)
        ]
    [r|^1.2.3||^2.0|]
      `shouldBe` Range
        [ Simple $ pure $ CaretRange (MajorMinorPatch 1 2 3),
          Simple $ pure $ CaretRange (MajorMinor 2 0)
        ]
    [r|>=1  <2|| >=3.0.0    || *|]
      `shouldBe` Range
        [ Simple $
            NE.fromList
              [ Primitive GreaterThanOrEqual (Major 1),
                Primitive LessThan (Major 2)
              ],
          Simple $ pure $ Primitive GreaterThanOrEqual (MajorMinorPatch 3 0 0),
          Simple $ pure $ Primitive Equal Any
        ]

  it "concatenating version ranges produces union of their comparator sets" $ do
    let r1 = [r|>1.0.0 || <2.0.0|]
    let r2 = [r|<2.0.0|]
    r1 <> r2 `shouldBe` r1

  describe "isVersionInRange" $ do
    let testRange range versionsWithResults =
          map (\(ver, _) -> isVersionInRange ver range) versionsWithResults
            `shouldBe` map snd versionsWithResults

    it "no version is in empty range" $
      testRange
        mempty
        [ ([v|0.5.5|], False),
          ([v|1.0.0|], False),
          ([v|1.2.3|], False),
          ([v|2.0.0|], False)
        ]
    it "complex range" $
      testRange
        [r|<=1.2.3 ^1.1.0 || 0.5.6|]
        [ ([v|0.5.5|], False),
          ([v|0.5.6|], True),
          ([v|0.5.7|], False),
          ([v|1.0.9|], False),
          ([v|1.1.0|], True),
          ([v|1.1.9|], True),
          ([v|1.2.3|], True),
          ([v|1.2.4|], False),
          ([v|1.3.0|], False),
          ([v|2.0.0|], False)
        ]

  describe "versionBounds" $ do
    let range ~> expectedInterval =
          it (show range) $ versionBounds range `shouldBe` expectedInterval

    [r||] ~> allVersionsInterval
    [r|>0.1.2|] ~> [vi| (0.1.2, inf) |]
    [r|>0.1.2 <0.2.0|] ~> [vi| (0.1.2, 0.2.0) |]
    [r|<=1.2.3|] ~> [vi| [0.0.0, 1.2.3] |]
    [r|^0.2.3|] ~> [vi| [0.2.3, 0.3.0) |]
    [r|^1.2.3|] ~> [vi| [1.2.3, 2.0.0) |]
    [r|<=1.2.3 ^1.1.0 || 0.5.6|] ~> [vi| [0.5.6, 1.2.3] |]

  describe "doesVersionRangeAllowMajorChanges" $ do
    let range ~> expected =
          it (show range) $
            doesVersionRangeAllowMajorChanges range `shouldBe` expected
    [r||] ~> True
    [r|>1.1.2|] ~> True
    [r|>0.1.2 <0.2.0|] ~> False
    [r|>0.1.2 <=0.2.0|] ~> True
    [r|>2.0.0 <= 3.0.0|] ~> True
    [r|>2.0.0 <3.0.0|] ~> False
    [r|<=2.9.99|] ~> True
    [r|^0.2.3|] ~> False
    [r|<=1.2.3 ^1.10 || 0.5.6|] ~> True
