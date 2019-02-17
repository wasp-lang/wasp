module Lib (
  someFunc,
  fibonacci
) where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

fibonacci :: (Num a, Ord a, Num b) => a -> b
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n | n > 1 = (fibonacci (n - 1)) + (fibonacci (n - 2))
fibonacci _ = undefined
