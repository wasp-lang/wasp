module Wasp.AppSpec.Core.Inspectable
  ( Inspectable (..),
    InspectionEntry (..),
    InspectionDatapoint,
    mapDatapointList,
  )
where

-- | Types that can describe themselves for `wasp inspect`.
class Inspectable a where
  inspect :: a -> [InspectionEntry]

data InspectionEntry = InspectionEntry
  { -- | The category heading for this inspection entry. This is used to group
    -- related data points together.
    heading :: String,
    -- | A list of (label, content) that represent the data points for this
    -- inspection entry.
    datapoints :: [InspectionDatapoint]
  }
  deriving (Show, Eq)

type InspectionDatapoint = (String, String)

mapDatapointList :: ([InspectionDatapoint] -> [InspectionDatapoint]) -> InspectionEntry -> InspectionEntry
mapDatapointList f entry@(InspectionEntry {datapoints}) =
  entry {datapoints = f datapoints}
