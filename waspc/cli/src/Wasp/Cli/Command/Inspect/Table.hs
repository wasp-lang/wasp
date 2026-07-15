{-# LANGUAGE TupleSections #-}

module Wasp.Cli.Command.Inspect.Table
  ( inspectAsTables,
  )
where

import Data.List (intercalate, nub)
import qualified Data.Map as Map
import Data.Maybe (catMaybes, fromMaybe)
import Wasp.AppSpec (AppSpec)
import Wasp.AppSpec.Core.Inspectable (InspectionDatapoint, InspectionEntry (..), inspect)
import Wasp.AppSpec.Inspectable (InspectableAppSpec (InspectableAppSpec))
import Wasp.Cli.Terminal (title)
import Wasp.Util (alignColumns, rightPad)
import qualified Wasp.Util.Terminal as Term

inspectAsTables :: AppSpec -> String
inspectAsTables sections = renderEntries $ inspect $ InspectableAppSpec sections

renderEntries :: [InspectionEntry] -> String
renderEntries (entries :: [InspectionEntry]) =
  intercalate "\n" (renderCategory <$> categoryPairs)
  where
    categoryPairs = groupWith heading datapoints entries
    renderCategory (title', datapointLists) =
      unlines $
        title title'
          : (("  " ++) <$> underlineFirstRow (alignColumns $ toRows datapointLists))

toRows :: [[InspectionDatapoint]] -> [[String]]
toRows datapointLists = columns : (toRow columns <$> datapointLists)
  where
    columns = nub $ fst <$> concat datapointLists

toRow :: [String] -> [InspectionDatapoint] -> [String]
toRow columns datapointList = [fromMaybe "" $ Map.lookup column rowMap | column <- columns]
  where
    rowMap = Map.fromList datapointList

groupWith :: (Ord k) => (a -> k) -> (a -> v) -> [a] -> [(k, [v])]
groupWith fk fv xs = catMaybes [(k,) <$> Map.lookup k asMap | k <- orderedKeys]
  where
    orderedKeys = nub $ fst <$> asPairs
    asMap = Map.fromListWith (++) asPairs
    asPairs = (\x -> (fk x, [fv x])) <$> xs

underlineFirstRow :: [String] -> [String]
underlineFirstRow [] = []
underlineFirstRow list@(firstRow : rest) = underlinedFirstRow : rest
  where
    maxLength = maximum $ length <$> list
    underlinedFirstRow =
      Term.applyStyles [Term.Underline] $
        rightPad ' ' maxLength firstRow
