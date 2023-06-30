{-# LANGUAGE DeriveGeneric #-}

module Wasp.Analyzer.Parser.SourceSpan
  ( SourceSpan (..),
    spansOverlap,
  )
where

import Control.DeepSeq (NFData)
import GHC.Generics (Generic)
import Wasp.Analyzer.Parser.SourceOffset

-- | @SourceSpan <startOffset> <endOffset>@
-- where @startOffset@ is source offset of the first character in the span while @endOffset@
-- is the source offset of the first character after the span.
-- So, if we have `app MyApp {` and we want to capture the `MyApp`,
-- we would do it with @SourceSpan 5 10@.
data SourceSpan = SourceSpan !SourceOffset !SourceOffset
  deriving (Eq, Ord, Show, Generic)

instance NFData SourceSpan

spansOverlap :: SourceSpan -> SourceSpan -> Bool
spansOverlap (SourceSpan s0 e0) (SourceSpan s1 e1)
  | s0 == e0 || s1 == e1 = False
  | otherwise = (s0 <= s1 && e0 > s1) || (s1 <= s0 && e1 > s0)
