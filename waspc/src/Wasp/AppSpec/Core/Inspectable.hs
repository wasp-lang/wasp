{-# LANGUAGE AllowAmbiguousTypes #-}

module Wasp.AppSpec.Core.Inspectable
  ( Inspectable (..),
    InspectionEntry (..),
  )
where

-- | Types that can describe themselves for `wasp inspect`.
class Inspectable a where
  -- | Heading of the section this type's entries are listed under, e.g. "Routes".
  inspectionSection :: String

  -- | Describes one named declaration as a single entry (row).
  inspect :: (String, a) -> InspectionEntry

-- | One row of the inspection output. The renderer aligns cells of the
-- entries in a section into columns; empty cells are allowed.
newtype InspectionEntry = InspectionEntry {entryCells :: [String]}
  deriving (Show, Eq)
