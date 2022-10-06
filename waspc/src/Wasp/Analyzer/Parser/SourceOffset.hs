module Wasp.Analyzer.Parser.SourceOffset
  ( SourceOffset,
  )
where

-- | If we look at source code as one big linear string where newlines are just normal characters,
-- so in just one dimension, then source offset is position in it, with 1 being the offset of the first character.
-- Example:
--   app MyApp {
--     title: "Hello"
--   }
--   Source position of row 2, column 3, which points to first letter of the "title", so "t", would translate into
--   source offset of 15.
--                                             |
--                                             v
--   1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19
--   a  p  p     M  y  A  p  p     { \n        t  i  t  l  e
type SourceOffset = Int
