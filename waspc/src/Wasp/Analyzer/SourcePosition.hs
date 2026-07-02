module Wasp.Analyzer.SourcePosition
  ( SourcePosition (..),
  )
where

import Data.Aeson (FromJSON (parseJSON), withObject, (.:))

-- | The first character on the first line is at position @Position 1 1@
-- @SourcePosition <line> <column>@
data SourcePosition = SourcePosition Int Int deriving (Eq)

instance FromJSON SourcePosition where
  parseJSON = withObject "SourcePosition" $ \v ->
    SourcePosition <$> v .: "line" <*> v .: "column"

instance Show SourcePosition where
  show (SourcePosition line column) = show line ++ ":" ++ show column
