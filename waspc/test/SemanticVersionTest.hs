module SemanticVersionTest where

import Data.List.NonEmpty (fromList)
import Numeric.Natural
import Test.Tasty.Hspec
import Wasp.SemanticVersion

spec_SemanticVersion :: Spec
spec_SemanticVersion = do
  describe "isVersionInRange" $ do
    it "Recognizes only version v to be in range '=v'" $
      testRange
        (rangeFromVersion (Equal, Version 1 2 3))
        [ ((0, 5, 5), False),
          ((1, 0, 0), False),
          ((1, 2, 3), True),
          ((1, 2, 4), False),
          ((1, 3, 0), False),
          ((2, 0, 0), False)
        ]
    it "Recognizes only versions lesser or equal to v to be in range '<=v'" $
      testRange
        (rangeFromVersion (LessThanOrEqual, Version 1 2 3))
        [ ((0, 5, 5), True),
          ((1, 0, 0), True),
          ((1, 2, 3), True),
          ((1, 2, 4), False),
          ((1, 3, 0), False),
          ((2, 0, 0), False)
        ]
    describe "Recognizes only versions >=v but smaller than next breaking change to be in range '^v'" $ do
      it "when v is of shape x.y.z where x != 0." $
        testRange
          (rangeFromVersion (BackwardsCompatibleWith, Version 1 2 3))
          [ ((0, 5, 5), False),
            ((1, 0, 0), False),
            ((1, 2, 3), True),
            ((1, 2, 4), True),
            ((1, 3, 0), True),
            ((2, 0, 0), False)
          ]
      it "when v is of shape 0.y.z where y != 0." $
        testRange
          (rangeFromVersion (BackwardsCompatibleWith, Version 0 2 3))
          [ ((0, 0, 0), False),
            ((0, 1, 3), False),
            ((0, 2, 0), False),
            ((0, 2, 2), False),
            ((0, 2, 3), True),
            ((0, 2, 4), True),
            ((0, 3, 0), False),
            ((1, 0, 0), False)
          ]
      it "when v is of shape 0.0.z." $
        testRange
          (rangeFromVersion (BackwardsCompatibleWith, Version 0 0 2))
          [ ((0, 0, 1), False),
            ((0, 0, 2), True),
            ((0, 0, 3), False),
            ((0, 1, 0), False),
            ((1, 0, 0), False)
          ]
      it "Correctly works for complex version range." $
        testRange
          ( ( rangeFromVersionsIntersection
                ( fromList
                    [ (LessThanOrEqual, Version 1 2 3),
                      (BackwardsCompatibleWith, Version 1 1 0)
                    ]
                )
            )
              <> rangeFromVersion (Equal, Version 0 5 6)
          )
          [ ((0, 5, 5), False),
            ((0, 5, 6), True),
            ((0, 5, 7), False),
            ((1, 0, 9), False),
            ((1, 1, 0), True),
            ((1, 1, 9), True),
            ((1, 2, 3), True),
            ((1, 2, 4), False),
            ((1, 3, 0), False),
            ((2, 0, 0), False)
          ]
  where
    testRange :: Range -> [((Natural, Natural, Natural), Bool)] -> Expectation
    testRange range versionsWithResults =
      ( (`isVersionInRange` range) . (\(x, y, z) -> Version x y z)
          <$> map fst versionsWithResults
      )
        `shouldBe` map snd versionsWithResults
