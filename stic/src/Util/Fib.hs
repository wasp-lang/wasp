module Util.Fib (
  fibonacci
) where

fibonacci :: (Num a, Ord a, Num b) => a -> b
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n | n > 1 = (fibonacci (n - 1)) + (fibonacci (n - 2))
fibonacci _ = undefined
