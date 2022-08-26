module TestUtil
  ( printDiff,
    shouldBeWithDiff,
    Diffable (toLines),
  )
where

import Data.Algorithm.Diff (Diff, PolyDiff (Both, First, Second), getDiff)
import Data.List (intercalate)
import Test.Tasty.Hspec (Expectation, expectationFailure)

shouldBeWithDiff :: (Eq a, Diffable a) => a -> a -> Expectation
shouldBeWithDiff actual expected
  | actual == expected = pure ()
  | otherwise =
    expectationFailure $
      "Actual is not expected\n"
        ++ printDiff (getDiff (toLines expected) (toLines actual))
        ++ "\ESC[31m"

printDiff :: [Diff String] -> String
printDiff diffs = intercalate "\n" $ map printDiffLine diffs
  where
    printDiffLine :: Diff String -> String
    printDiffLine (First s) = "  \ESC[31m- " ++ s
    printDiffLine (Second s) = "  \ESC[36m+ " ++ s
    printDiffLine (Both s _) = "  \ESC[0m  " ++ s

class Diffable a where
  toLines :: a -> [String]

instance Diffable Char where
  toLines c = [[c]]

instance Diffable a => Diffable [a] where
  toLines xs = concatMap toLines xs

instance (Diffable a, Diffable b) => Diffable (a, b) where
  toLines (a, b) = toLines a ++ toLines b
