{-# LANGUAGE DeriveGeneric #-}

module Wasp.Analyzer.Parser.SourceSpan
  ( SourceSpan (..),
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
