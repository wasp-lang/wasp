module SemanticVersion.RangeTest where

import Data.Either (isLeft)
import qualified Data.List.NonEmpty as NE
import Test.Hspec
import qualified Text.Parsec as P
import Wasp.SemanticVersion.PartialVersion
import Wasp.SemanticVersion.Range
import Wasp.SemanticVersion.RangeExpression
import Wasp.SemanticVersion.Version
import Wasp.SemanticVersion.VersionBound

spec_SemanticVersion_Range :: Spec
spec_SemanticVersion_Range = do
  it "show" $ do
    show [r|<=1.3.6|] `shouldBe` "<=1.3.6"
    show [r|<=1.3.6 ^1.2.0 || 1.2.3|] `shouldBe` "<=1.3.6 ^1.2.0 || 1.2.3"
    show [r|<=1.3.6 ^1.2.0 || 1.2.3 || 1 - 2 || ^5|] `shouldBe` "<=1.3.6 ^1.2.0 || 1.2.3 || 1 - 2 || ^5"
    show [r|>=1.2.3-alpha.1 <1.2.3 || 1.2.3-pre+asdf - 2.4.3-pre || ^2.0.0-rc.1+build|]
      `shouldBe` ">=1.2.3-alpha.1 <1.2.3 || 1.2.3-pre+asdf - 2.4.3-pre || ^2.0.0-rc.1+build"

  describe "parseRange" $ do
    let looseParseRange = P.parse rangeParser ""
    let strictParseRange = parseRange

    it "parses range with single a range expression" $ do
      strictParseRange "  >=1.0 <2.0.0  "
        `shouldBe` Right
          ( Range $
              NE.fromList
                [ SimpleRangeExpressionSet $
                    NE.fromList
                      [ PrimitiveRangeExpression GreaterThanOrEqual (MajorMinor 1 0),
                        PrimitiveRangeExpression LessThan (FullVersion [v|2.0.0|])
                      ]
                ]
          )
      strictParseRange " ^1.2.3  "
        `shouldBe` Right
          ( Range $
              NE.fromList
                [ SimpleRangeExpressionSet $ pure $ CaretRangeExpression (FullVersion [v|1.2.3|])
                ]
          )

    it "parses range with multiple range expressions" $ do
      strictParseRange "   ^1.2.3 ||   ^2.0 ||"
        `shouldBe` Right
          ( Range $
              NE.fromList
                [ SimpleRangeExpressionSet $ pure $ CaretRangeExpression (FullVersion [v|1.2.3|]),
                  SimpleRangeExpressionSet $ pure $ CaretRangeExpression (MajorMinor 2 0),
                  SimpleRangeExpressionSet $ pure $ PrimitiveRangeExpression Equal Any
                ]
          )
      strictParseRange "^1.2.3||^2.0     "
        `shouldBe` Right
          ( Range $
              NE.fromList
                [ SimpleRangeExpressionSet $ pure $ CaretRangeExpression (FullVersion [v|1.2.3|]),
                  SimpleRangeExpressionSet $ pure $ CaretRangeExpression (MajorMinor 2 0)
                ]
          )
      strictParseRange ">=1  <2|| >=3.0.0    || *  "
        `shouldBe` Right
          ( Range $
              NE.fromList
                [ SimpleRangeExpressionSet $
                    NE.fromList
                      [ PrimitiveRangeExpression GreaterThanOrEqual (Major 1),
                        PrimitiveRangeExpression LessThan (Major 2)
                      ],
                  SimpleRangeExpressionSet $ pure $ PrimitiveRangeExpression GreaterThanOrEqual (FullVersion [v|3.0.0|]),
                  SimpleRangeExpressionSet $ pure $ PrimitiveRangeExpression Equal Any
                ]
          )

    it "parses range with trailing content" $ do
      looseParseRange "^1.2.3 || ^2.0 ||a"
        `shouldBe` Right
          ( Range $
              NE.fromList
                [ SimpleRangeExpressionSet $ pure $ CaretRangeExpression (FullVersion [v|1.2.3|]),
                  SimpleRangeExpressionSet $ pure $ CaretRangeExpression (MajorMinor 2 0)
                ]
          )
      looseParseRange "^1.2.3 || ^2.0 abc"
        `shouldBe` Right
          ( Range $
              NE.fromList
                [ SimpleRangeExpressionSet $ pure $ CaretRangeExpression (FullVersion [v|1.2.3|]),
                  SimpleRangeExpressionSet $ pure $ CaretRangeExpression (MajorMinor 2 0)
                ]
          )

    it "parses range with pre-release and build versions" $ do
      strictParseRange ">=1.2.3-alpha.1 <1.2.3 || ^2.0.0-rc.1+build"
        `shouldBe` Right
          ( Range $
              NE.fromList
                [ SimpleRangeExpressionSet $
                    NE.fromList
                      [ PrimitiveRangeExpression GreaterThanOrEqual (FullVersion [v|1.2.3-alpha.1|]),
                        PrimitiveRangeExpression LessThan (FullVersion [v|1.2.3|])
                      ],
                  SimpleRangeExpressionSet $ pure $ CaretRangeExpression (FullVersion [v|2.0.0-rc.1|])
                ]
          )

    it "rejects invalid formats" $ do
      isLeft (strictParseRange "foo") `shouldBe` True
      isLeft (strictParseRange "|| 1.23 || 2.0") `shouldBe` True
      isLeft (strictParseRange "1.23 || $2.0") `shouldBe` True
      isLeft (strictParseRange "1.23 || 1.0 ||a") `shouldBe` True
      isLeft (strictParseRange "^1.2-alpha || 2.0.0") `shouldBe` True
      isLeft (strictParseRange "1.2.3-01 || 2.0.0") `shouldBe` True

  it "r quasi quoter" $ do
    [r|^1.2.3 ||   ^2.0|]
      `shouldBe` Range
        ( NE.fromList
            [ SimpleRangeExpressionSet $ pure $ CaretRangeExpression (FullVersion [v|1.2.3|]),
              SimpleRangeExpressionSet $ pure $ CaretRangeExpression (MajorMinor 2 0)
            ]
        )
    [r|^1.2.3||^2.0|]
      `shouldBe` Range
        ( NE.fromList
            [ SimpleRangeExpressionSet $ pure $ CaretRangeExpression (FullVersion [v|1.2.3|]),
              SimpleRangeExpressionSet $ pure $ CaretRangeExpression (MajorMinor 2 0)
            ]
        )
    [r|>=1  <2|| >=3.0.0    || *|]
      `shouldBe` Range
        ( NE.fromList
            [ SimpleRangeExpressionSet $
                NE.fromList
                  [ PrimitiveRangeExpression GreaterThanOrEqual (Major 1),
                    PrimitiveRangeExpression LessThan (Major 2)
                  ],
              SimpleRangeExpressionSet $ pure $ PrimitiveRangeExpression GreaterThanOrEqual (FullVersion [v|3.0.0|]),
              SimpleRangeExpressionSet $ pure $ PrimitiveRangeExpression Equal Any
            ]
        )

  it "concatenating ranges should produce union of their range expressions" $ do
    let r1 = [r|>1.0.0 || <2.0.0|]
    let r2 = [r|<2.0.0|]
    r1 <> r2 `shouldBe` r1

  describe "helper constructors" $ do
    it "keep pre-release and build" $ do
      show (caretRange [v|1.2.3-alpha+build|]) `shouldBe` "^1.2.3-alpha+build"
      show (tildeRange [v|1.2.3-alpha|]) `shouldBe` "~1.2.3-alpha"
      show (hyphenRange [v|1.2.3-alpha|] [v|2.0.0-rc.1|]) `shouldBe` "1.2.3-alpha - 2.0.0-rc.1"
      show (lt [v|1.2.3-alpha|]) `shouldBe` "<1.2.3-alpha"
      show (lte [v|1.2.3-alpha|]) `shouldBe` "<=1.2.3-alpha"
      show (gt [v|1.2.3-alpha|]) `shouldBe` ">1.2.3-alpha"
      show (gte [v|1.2.3-alpha|]) `shouldBe` ">=1.2.3-alpha"
      show (eq [v|1.2.3-alpha|]) `shouldBe` "1.2.3-alpha"

  describe "isVersionInRange" $ do
    let testRange range versionsWithResults =
          map (\(ver, _) -> isVersionInRange ver range) versionsWithResults
            `shouldBe` map snd versionsWithResults

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

    it "pre-release versions" $
      testRange
        [r|^1.2.3-beta.2 || 2.0.0-rc.1 - 2.0.0|]
        [ ([v|1.2.3-alpha|], False),
          ([v|1.2.3-beta.1|], False),
          ([v|1.2.3-beta.2|], True),
          ([v|1.2.3-beta.10|], True),
          ([v|1.2.3-rc|], True),
          ([v|1.2.3|], True),
          ([v|1.2.4-beta.2|], False),
          ([v|1.9.9|], True),
          ([v|2.0.0-0|], False),
          ([v|2.0.0-beta|], False),
          ([v|2.0.0-rc.1|], True),
          ([v|2.0.0-rc.2|], True),
          ([v|2.0.0|], True),
          ([v|2.0.1-rc.1|], False),
          ([v|2.0.1|], False)
        ]

    it "build" $
      testRange
        [r|1.2.3+build.1 || ^2.0.0-rc+build|]
        [ ([v|1.2.3|], True),
          ([v|1.2.3+build.1|], True),
          ([v|1.2.3+build.2|], True),
          ([v|1.2.4+build.1|], False),
          ([v|2.0.0-rc+other|], True),
          ([v|2.1.0+other|], True)
        ]

    -- Cases from `node-semver`'s test/fixtures/range-include.js and test/fixtures/range-exclude.js.
    -- We skip the cases with options (e.g. loose, includePrerelease) and syntax we don't support
    -- (e.g. `~>`, the `v` prefix, a leading `||`, tabs).
    describe "matches node-semver" $ do
      let (range, version) ~> expected =
            it (show version <> (if expected then " ∈ " else " ∉ ") <> show range) $
              isVersionInRange version range `shouldBe` expected

      ([r|1.0.0 - 2.0.0|], [v|1.2.3|]) ~> True
      ([r|^1.2.3+build|], [v|1.2.3|]) ~> True
      ([r|^1.2.3+build|], [v|1.3.0|]) ~> True
      ([r|1.2.3-pre+asdf - 2.4.3-pre+asdf|], [v|1.2.3|]) ~> True
      ([r|1.2.3-pre+asdf - 2.4.3-pre+asdf|], [v|1.2.3-pre.2|]) ~> True
      ([r|1.2.3-pre+asdf - 2.4.3-pre+asdf|], [v|2.4.3-alpha|]) ~> True
      ([r|1.2.3+asdf - 2.4.3+asdf|], [v|1.2.3|]) ~> True
      ([r|1.0.0|], [v|1.0.0|]) ~> True
      ([r|>=*|], [v|0.2.4|]) ~> True
      ([r||], [v|1.0.0|]) ~> True
      ([r|>1.0.0|], [v|1.1.0|]) ~> True
      ([r|<=2.0.0|], [v|2.0.0|]) ~> True
      ([r|<=2.0.0|], [v|1.9999.9999|]) ~> True
      ([r|<=2.0.0|], [v|0.2.9|]) ~> True
      ([r|<2.0.0|], [v|1.9999.9999|]) ~> True
      ([r|<2.0.0|], [v|0.2.9|]) ~> True
      ([r|>= 1.0.0|], [v|1.0.0|]) ~> True
      ([r|>=  1.0.0|], [v|1.0.1|]) ~> True
      ([r|>=   1.0.0|], [v|1.1.0|]) ~> True
      ([r|> 1.0.0|], [v|1.0.1|]) ~> True
      ([r|>  1.0.0|], [v|1.1.0|]) ~> True
      ([r|<=   2.0.0|], [v|2.0.0|]) ~> True
      ([r|<= 2.0.0|], [v|1.9999.9999|]) ~> True
      ([r|<=  2.0.0|], [v|0.2.9|]) ~> True
      ([r|<    2.0.0|], [v|1.9999.9999|]) ~> True
      ([r|>=0.1.97|], [v|0.1.97|]) ~> True
      ([r|0.1.20 || 1.2.4|], [v|1.2.4|]) ~> True
      ([r|>=0.2.3 || <0.0.1|], [v|0.0.0|]) ~> True
      ([r|>=0.2.3 || <0.0.1|], [v|0.2.3|]) ~> True
      ([r|>=0.2.3 || <0.0.1|], [v|0.2.4|]) ~> True
      ([r|2.x.x|], [v|2.1.3|]) ~> True
      ([r|1.2.x|], [v|1.2.3|]) ~> True
      ([r|1.2.x || 2.x|], [v|2.1.3|]) ~> True
      ([r|1.2.x || 2.x|], [v|1.2.3|]) ~> True
      ([r|x|], [v|1.2.3|]) ~> True
      ([r|2.*.*|], [v|2.1.3|]) ~> True
      ([r|1.2.*|], [v|1.2.3|]) ~> True
      ([r|1.2.* || 2.*|], [v|2.1.3|]) ~> True
      ([r|1.2.* || 2.*|], [v|1.2.3|]) ~> True
      ([r|*|], [v|1.2.3|]) ~> True
      ([r|2|], [v|2.1.2|]) ~> True
      ([r|2.3|], [v|2.3.1|]) ~> True
      ([r|~0.0.1|], [v|0.0.1|]) ~> True
      ([r|~0.0.1|], [v|0.0.2|]) ~> True
      ([r|~x|], [v|0.0.9|]) ~> True
      ([r|~2|], [v|2.0.9|]) ~> True
      ([r|~2.4|], [v|2.4.0|]) ~> True
      ([r|~2.4|], [v|2.4.5|]) ~> True
      ([r|~1|], [v|1.2.3|]) ~> True
      ([r|~1.0|], [v|1.0.2|]) ~> True
      ([r|~ 1.0|], [v|1.0.2|]) ~> True
      ([r|~ 1.0.3|], [v|1.0.12|]) ~> True
      ([r|>=1|], [v|1.0.0|]) ~> True
      ([r|>= 1|], [v|1.0.0|]) ~> True
      ([r|<1.2|], [v|1.1.1|]) ~> True
      ([r|< 1.2|], [v|1.1.1|]) ~> True
      ([r|=0.7.x|], [v|0.7.2|]) ~> True
      ([r|<=0.7.x|], [v|0.7.2|]) ~> True
      ([r|>=0.7.x|], [v|0.7.2|]) ~> True
      ([r|<=0.7.x|], [v|0.6.2|]) ~> True
      ([r|~1.2.1 >=1.2.3|], [v|1.2.3|]) ~> True
      ([r|~1.2.1 =1.2.3|], [v|1.2.3|]) ~> True
      ([r|~1.2.1 1.2.3|], [v|1.2.3|]) ~> True
      ([r|~1.2.1 >=1.2.3 1.2.3|], [v|1.2.3|]) ~> True
      ([r|~1.2.1 1.2.3 >=1.2.3|], [v|1.2.3|]) ~> True
      ([r|>=1.2.1 1.2.3|], [v|1.2.3|]) ~> True
      ([r|1.2.3 >=1.2.1|], [v|1.2.3|]) ~> True
      ([r|>=1.2.3 >=1.2.1|], [v|1.2.3|]) ~> True
      ([r|>=1.2.1 >=1.2.3|], [v|1.2.3|]) ~> True
      ([r|>=1.2|], [v|1.2.8|]) ~> True
      ([r|^1.2.3|], [v|1.8.1|]) ~> True
      ([r|^0.1.2|], [v|0.1.2|]) ~> True
      ([r|^0.1|], [v|0.1.2|]) ~> True
      ([r|^0.0.1|], [v|0.0.1|]) ~> True
      ([r|^1.2|], [v|1.4.2|]) ~> True
      ([r|^1.2 ^1|], [v|1.4.2|]) ~> True
      ([r|^1.2.3-alpha|], [v|1.2.3-pre|]) ~> True
      ([r|^1.2.0-alpha|], [v|1.2.0-pre|]) ~> True
      ([r|^0.0.1-alpha|], [v|0.0.1-beta|]) ~> True
      ([r|^0.0.1-alpha|], [v|0.0.1|]) ~> True
      ([r|^0.1.1-alpha|], [v|0.1.1-beta|]) ~> True
      ([r|^x|], [v|1.2.3|]) ~> True
      ([r|x - 1.0.0|], [v|0.9.7|]) ~> True
      ([r|x - 1.x|], [v|0.9.7|]) ~> True
      ([r|1.0.0 - x|], [v|1.9.7|]) ~> True
      ([r|1.x - x|], [v|1.9.7|]) ~> True
      ([r|<=7.x|], [v|7.9.9|]) ~> True
      ([r|1.0.0 - 2.0.0|], [v|2.2.3|]) ~> False
      ([r|1.2.3+asdf - 2.4.3+asdf|], [v|1.2.3-pre.2|]) ~> False
      ([r|1.2.3+asdf - 2.4.3+asdf|], [v|2.4.3-alpha|]) ~> False
      ([r|^1.2.3+build|], [v|2.0.0|]) ~> False
      ([r|^1.2.3+build|], [v|1.2.0|]) ~> False
      ([r|^1.2.3|], [v|1.2.3-pre|]) ~> False
      ([r|^1.2|], [v|1.2.0-pre|]) ~> False
      ([r|>1.2|], [v|1.3.0-beta|]) ~> False
      ([r|<=1.2.3|], [v|1.2.3-beta|]) ~> False
      ([r|^1.2.3|], [v|1.2.3-beta|]) ~> False
      ([r|=0.7.x|], [v|0.7.0-asdf|]) ~> False
      ([r|>=0.7.x|], [v|0.7.0-asdf|]) ~> False
      ([r|<=0.7.x|], [v|0.7.0-asdf|]) ~> False
      ([r|1.0.0|], [v|1.0.1|]) ~> False
      ([r|>=1.0.0|], [v|0.0.0|]) ~> False
      ([r|>=1.0.0|], [v|0.0.1|]) ~> False
      ([r|>=1.0.0|], [v|0.1.0|]) ~> False
      ([r|>1.0.0|], [v|0.0.1|]) ~> False
      ([r|>1.0.0|], [v|0.1.0|]) ~> False
      ([r|<=2.0.0|], [v|3.0.0|]) ~> False
      ([r|<=2.0.0|], [v|2.9999.9999|]) ~> False
      ([r|<=2.0.0|], [v|2.2.9|]) ~> False
      ([r|<2.0.0|], [v|2.9999.9999|]) ~> False
      ([r|<2.0.0|], [v|2.2.9|]) ~> False
      ([r|>=0.1.97|], [v|0.1.93|]) ~> False
      ([r|0.1.20 || 1.2.4|], [v|1.2.3|]) ~> False
      ([r|>=0.2.3 || <0.0.1|], [v|0.0.3|]) ~> False
      ([r|>=0.2.3 || <0.0.1|], [v|0.2.2|]) ~> False
      ([r|2.x.x|], [v|3.1.3|]) ~> False
      ([r|1.2.x|], [v|1.3.3|]) ~> False
      ([r|1.2.x || 2.x|], [v|3.1.3|]) ~> False
      ([r|1.2.x || 2.x|], [v|1.1.3|]) ~> False
      ([r|2.*.*|], [v|1.1.3|]) ~> False
      ([r|2.*.*|], [v|3.1.3|]) ~> False
      ([r|1.2.*|], [v|1.3.3|]) ~> False
      ([r|1.2.* || 2.*|], [v|3.1.3|]) ~> False
      ([r|1.2.* || 2.*|], [v|1.1.3|]) ~> False
      ([r|2|], [v|1.1.2|]) ~> False
      ([r|2.3|], [v|2.4.1|]) ~> False
      ([r|~0.0.1|], [v|0.1.0-alpha|]) ~> False
      ([r|~0.0.1|], [v|0.1.0|]) ~> False
      ([r|~2.4|], [v|2.5.0|]) ~> False
      ([r|~2.4|], [v|2.3.9|]) ~> False
      ([r|~1|], [v|0.2.3|]) ~> False
      ([r|~1.0|], [v|1.1.0|]) ~> False
      ([r|<1|], [v|1.0.0|]) ~> False
      ([r|>=1.2|], [v|1.1.1|]) ~> False
      ([r|=0.7.x|], [v|0.8.2|]) ~> False
      ([r|>=0.7.x|], [v|0.6.2|]) ~> False
      ([r|<0.7.x|], [v|0.7.2|]) ~> False
      ([r|<1.2.3|], [v|1.2.3-beta|]) ~> False
      ([r|=1.2.3|], [v|1.2.3-beta|]) ~> False
      ([r|>1.2|], [v|1.2.8|]) ~> False
      ([r|^0.0.1|], [v|0.0.2-alpha|]) ~> False
      ([r|^0.0.1|], [v|0.0.2|]) ~> False
      ([r|^1.2.3|], [v|2.0.0-alpha|]) ~> False
      ([r|^1.2.3|], [v|1.2.2|]) ~> False
      ([r|^1.2|], [v|1.1.9|]) ~> False
      ([r|^1.0.0|], [v|2.0.0-rc1|]) ~> False
      ([r|1 - 2|], [v|2.0.0-pre|]) ~> False
      ([r|1 - 2|], [v|1.0.0-pre|]) ~> False
      ([r|1.0 - 2|], [v|1.0.0-pre|]) ~> False
      ([r|1.1.x|], [v|1.0.0-a|]) ~> False
      ([r|1.1.x|], [v|1.1.0-a|]) ~> False
      ([r|1.1.x|], [v|1.2.0-a|]) ~> False
      ([r|1.x|], [v|1.0.0-a|]) ~> False
      ([r|1.x|], [v|1.1.0-a|]) ~> False
      ([r|1.x|], [v|1.2.0-a|]) ~> False
      ([r|>=1.0.0 <1.1.0|], [v|1.1.0|]) ~> False
      ([r|>=1.0.0 <1.1.0|], [v|1.1.0-pre|]) ~> False
      ([r|>=1.0.0 <1.1.0-pre|], [v|1.1.0-pre|]) ~> False

  describe "versionBounds" $ do
    let range ~> expectedInterval =
          it (show range) $ versionBounds range `shouldBe` expectedInterval

    [r||] ~> allVersionsInterval
    [r|>0.1.2|] ~> [vi| (0.1.2, inf) |]
    [r|>0.1.2 <0.2.0|] ~> [vi| (0.1.2, 0.2.0) |]
    [r|<=1.2.3|] ~> [vi| [0.0.0-0, 1.2.3] |]
    [r|^0.2.3|] ~> [vi| [0.2.3, 0.3.0-0) |]
    [r|^1.2.3|] ~> [vi| [1.2.3, 2.0.0-0) |]
    [r|<=1.2.3 ^1.1.0 || 0.5.6|] ~> [vi| [0.5.6, 1.2.3] |]
    [r|^1.2.3-beta.2|] ~> [vi| [1.2.3-beta.2, 2.0.0-0) |]
    [r|>=1.2.3-alpha <1.2.3 || 1.2.3-pre - 2.4|] ~> [vi| [1.2.3-alpha, 2.5.0-0) |]

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
    [r|^1.2.3-alpha|] ~> False
    [r|~0.0.1-alpha|] ~> True
    [r|>=1.0.0-rc.1 <2.0.0|] ~> False
    [r|>=1.0.0 <2.0.0-0|] ~> False
    [r|>=1.0.0 <=2.0.0|] ~> True
    [r|^1.2.3-alpha || ^2.0.0-alpha|] ~> True
