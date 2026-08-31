{-# LANGUAGE TupleSections #-}

module Wasp.Cli.Command.Show.Table
  ( renderEntriesAsTables,
  )
where

import Data.List (intercalate, nub, sortOn)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe, mapMaybe)
import Wasp.Cli.Terminal (title)
import Wasp.Inspectable (InspectionDatapoint, InspectionEntry (..))
import Wasp.Util (alignColumns)
import qualified Wasp.Util.Terminal as Term

-- | Renders inspection entries as per-category tables: entries are grouped by
-- their heading, and each group becomes a table with one row per entry.
renderEntriesAsTables :: [InspectionEntry] -> String
renderEntriesAsTables entries =
  intercalate "\n" (renderCategory . fmap sortEntriesByFirstColumnValue <$> categoryPairs)
  where
    categoryPairs = orderedGroupWith heading datapoints entries

    sortEntriesByFirstColumnValue :: [[InspectionDatapoint]] -> [[InspectionDatapoint]]
    sortEntriesByFirstColumnValue = sortOn $ \case
      [] -> ""
      ((_, firstColumnValue) : _) -> firstColumnValue

    renderCategory (title', datapointLists) =
      unlines $
        title title'
          : ( ("  " ++)
                <$>
                -- We run the "underline" _after_ aligning columns, as the
                -- underline ANSI sequences otherwise get counted in the column
                -- width calculation and throw off the alignment.
                underlineFirstRow (alignColumns $ toRows datapointLists)
            )

toRows :: [[InspectionDatapoint]] -> [[String]]
toRows datapointLists = columns : (toRow columns <$> datapointLists)
  where
    columns = nub $ fst <$> concat datapointLists

toRow :: [String] -> [InspectionDatapoint] -> [String]
toRow columns datapointList = [fromMaybe "" $ Map.lookup column rowMap | column <- columns]
  where
    rowMap = Map.fromList datapointList

orderedGroupWith :: (Ord k) => (a -> k) -> (a -> v) -> [a] -> [(k, [v])]
orderedGroupWith fk fv xs =
  mapMaybe lookupPair orderedUniqueKeys
  where
    orderedUniqueKeys = nub $ fst <$> asPairs

    lookupPair k = (k,) <$> Map.lookup k asGroupedMap
    asGroupedMap = Map.fromListWith (++) asPairs

    asPairs = (\x -> (fk x, [fv x])) <$> xs

underlineFirstRow :: [String] -> [String]
underlineFirstRow [] = []
underlineFirstRow (firstRow : rest) =
  Term.applyStyles [Term.Underline] firstRow : rest
