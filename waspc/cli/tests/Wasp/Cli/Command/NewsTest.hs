{-# LANGUAGE OverloadedRecordDot #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Wasp.Cli.Command.NewsTest where

import qualified Data.Set as Set
import Data.Time (UTCTime)
import qualified Data.Time as T
import Test.Tasty.QuickCheck
import Wasp.Cli.Command.News (NewsReport (..), getAutomaticNewsReport)
import Wasp.Cli.Command.News.Common (NewsEntry (..), NewsLevel (..))
import Wasp.Cli.Command.News.Persistence (LocalNewsInfo (..))

-- * Arbitrary instances

instance Arbitrary NewsLevel where
  arbitrary = elements [Low, Moderate, High]

instance Arbitrary NewsEntry where
  arbitrary =
    NewsEntry
      <$> arbitrary -- id
      <*> arbitrary -- title
      <*> arbitrary -- body
      <*> arbitrary -- level
      <*> arbitraryUTCTime -- publishedAt

arbitraryUTCTime :: Gen UTCTime
arbitraryUTCTime = do
  day <- T.fromGregorian <$> choose (2020, 2025) <*> choose (1, 12) <*> choose (1, 28)
  seconds <- T.secondsToDiffTime <$> choose (0, 86399)
  return $ T.UTCTime day seconds

instance Arbitrary LocalNewsInfo where
  arbitrary =
    LocalNewsInfo
      <$> (Just <$> arbitraryUTCTime)
      <*> (Set.fromList <$> arbitrary)

-- * Property tests

prop_allShownNewsAreAtLeastModerate :: LocalNewsInfo -> [NewsEntry] -> Bool
prop_allShownNewsAreAtLeastModerate localNewsInfo newsEntries =
  all ((>= Moderate) . (.level)) report.newsToShow
  where
    report = getAutomaticNewsReport localNewsInfo newsEntries

prop_noShownNewsWasPreviouslySeen :: LocalNewsInfo -> [NewsEntry] -> Bool
prop_noShownNewsWasPreviouslySeen localNewsInfo newsEntries =
  not $ any ((`Set.member` localNewsInfo.seenNewsIds) . (.id)) report.newsToShow
  where
    report = getAutomaticNewsReport localNewsInfo newsEntries

prop_requireConfirmationIffHighPriorityShown :: LocalNewsInfo -> [NewsEntry] -> Bool
prop_requireConfirmationIffHighPriorityShown localNewsInfo newsEntries =
  report.requireConfirmation == any ((== High) . (.level)) report.newsToShow
  where
    report = getAutomaticNewsReport localNewsInfo newsEntries

prop_markAllAsSeenIfRequiringConfirmation :: LocalNewsInfo -> [NewsEntry] -> Bool
prop_markAllAsSeenIfRequiringConfirmation localNewsInfo newsEntries =
  not report.requireConfirmation || report.newsToConsiderSeen == report.newsToShow
  where
    report = getAutomaticNewsReport localNewsInfo newsEntries

prop_markNothingAsSeenIfNoConfirmation :: LocalNewsInfo -> [NewsEntry] -> Bool
prop_markNothingAsSeenIfNoConfirmation localNewsInfo newsEntries =
  report.requireConfirmation || null report.newsToConsiderSeen
  where
    report = getAutomaticNewsReport localNewsInfo newsEntries

-- TODO: This test is basically the implementation
prop_allRelevantUnseenNewsAreShown :: LocalNewsInfo -> [NewsEntry] -> Bool
prop_allRelevantUnseenNewsAreShown localNewsInfo newsEntries =
  all (`elem` report.newsToShow) relevantUnseenNews
  where
    report = getAutomaticNewsReport localNewsInfo newsEntries
    relevantUnseenNews = filter isRelevant . filter isUnseen $ newsEntries
    isRelevant = (>= Moderate) . (.level)
    isUnseen entry = entry.id `Set.notMember` localNewsInfo.seenNewsIds
