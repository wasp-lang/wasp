{-# LANGUAGE DeriveGeneric #-}

module Wasp.Analyzer.Parser.ConcreteParser.ParseError
  ( -- * Parse error
    ParseError (..),

    -- * Source positions
    Region (..),
  )
where

import Control.DeepSeq (NFData)
import GHC.Generics (Generic)
import Wasp.Analyzer.Parser.Token (TokenKind)
import Wasp.Analyzer.Parser.TokenSet (TokenSet)

data ParseError
  = UnexpectedToken !Region !TokenKind TokenSet
  | UnexpectedEOF !Int TokenSet
  deriving (Eq, Ord, Show, Generic)

instance NFData ParseError

-- | @Region start end@ where @start@ is the offset of the first character in
-- the region and @end@ is the offset of the first character after the region.
-- In other words, its the region of characters with offsets from @start@ to
-- @end@, including @start@ but not including @end@.
data Region = Region !Int !Int deriving (Eq, Ord, Show, Generic)

instance NFData Region
