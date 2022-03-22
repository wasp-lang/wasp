module SemanticVersionTest where

import Test.Tasty.Hspec
import Wasp.SemanticVersion

spec_isVersionInBounds :: Spec
spec_isVersionInBounds = do
  it "Correctly determines satisfied exact version bounds" $
    Version 1 2 3 `isVersionInBounds` Exact (Version 1 2 3)
  it "Correctly determines unsatisfied exact version bounds" $
    not $
      any
        (`isVersionInBounds` Exact (Version 1 2 3))
        [Version 2 2 3, Version 1 3 3, Version 1 2 4]
  it "Correctly determine satisfied backwards compatible version bounds" $
    all
      (`isVersionInBounds` BackwardsCompatibleWith (Version 1 2 3))
      [Version 1 2 3, Version 1 2 4, Version 1 3 0]
  it "Correctly determines unsatisfied backwards compatible version bounds" $
    not $
      any
        (`isVersionInBounds` BackwardsCompatibleWith (Version 1 2 3))
        [Version 1 2 0, Version 1 2 2, Version 2 0 0]
  it "Correctly determines satisfied backwards compatible version bounds with 0.x versions" $
    all
      (`isVersionInBounds` BackwardsCompatibleWith (Version 0 2 3))
      [Version 0 2 3, Version 0 2 4]
  it "Correctly determines unsatisfied backwards compatible version bounds with 0.x versions" $
    not $
      any
        (`isVersionInBounds` BackwardsCompatibleWith (Version 0 2 3))
        [Version 0 0 0, Version 0 1 3, Version 0 2 0, Version 0 2 2, Version 0 3 0, Version 1 0 0]
  it "Correctly determines satisfied backwards compatible version bounds with 0.0.x versions" $
    Version 0 0 2 `isVersionInBounds` BackwardsCompatibleWith (Version 0 0 2)
  it "Correctly determines unsatisfied backwards compatible version bounds with 0.0.x versions" $
    not $
      any
        (`isVersionInBounds` BackwardsCompatibleWith (Version 0 0 2))
        [Version 0 0 1, Version 0 0 3, Version 0 1 0, Version 1 0 0]
