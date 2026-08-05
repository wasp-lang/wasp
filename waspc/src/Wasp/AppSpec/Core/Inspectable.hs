module Wasp.AppSpec.Core.Inspectable
  ( InspectionEntry (..),
    InspectionDatapoint,
  )
where

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
