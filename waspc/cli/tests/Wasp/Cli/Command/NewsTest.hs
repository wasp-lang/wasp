{-# LANGUAGE OverloadedRecordDot #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Wasp.Cli.Command.NewsTest where

import qualified Data.Set as Set
import qualified Data.Time as T
import Test.Tasty.QuickCheck
import Wasp.Cli.Command.News (NewsReport (..), getAutomaticNewsReport)
import Wasp.Cli.Command.News.Common (NewsEntry (..), NewsLevel (..))
import Wasp.Cli.Command.News.Persistence (LocalNewsInfo (..))

instance Arbitrary NewsLevel where
  arbitrary = elements [Low, Moderate, High]

instance Arbitrary NewsEntry where
  arbitrary =
    NewsEntry
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitraryUTCTime

instance Arbitrary LocalNewsInfo where
  arbitrary =
    LocalNewsInfo
      <$> (Just <$> arbitraryUTCTime)
      <*> (Set.fromList <$> arbitrary)

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

prop_dontMarkAnyAsSeenIfNoConfirmation :: LocalNewsInfo -> [NewsEntry] -> Bool
prop_dontMarkAnyAsSeenIfNoConfirmation localNewsInfo newsEntries =
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

arbitraryUTCTime :: Gen T.UTCTime
arbitraryUTCTime = T.UTCTime <$> arbiraryDay
  where
    arbitraryDay = T.fromGregorian <$> choose (2020, 2025) <*> choose (1, 12) <*> choose (1, 28)
