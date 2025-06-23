module Wasp.Util.Random
  ( genRandomAsciiAlphaNums,
    genRandomAsciiAlphaNumsIO,
  )
where

import Data.Char (isAsciiLower, isAsciiUpper, isDigit)
import System.Random (Random (randoms), RandomGen, newStdGen)

genRandomAsciiAlphaNums :: RandomGen g => Int -> g -> [Char]
genRandomAsciiAlphaNums len gen = take len $ filter isAsciiAlphaNum $ randoms gen
  where
    isAsciiAlphaNum c = isAsciiUpper c || isAsciiLower c || isDigit c

genRandomAsciiAlphaNumsIO :: Int -> IO [Char]
genRandomAsciiAlphaNumsIO len = genRandomAsciiAlphaNums len <$> newStdGen
