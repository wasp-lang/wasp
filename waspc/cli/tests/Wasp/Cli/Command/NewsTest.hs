{-# LANGUAGE OverloadedRecordDot #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Wasp.Cli.Command.NewsTest where

import Data.Maybe (isNothing)
import qualified Data.Set as Set
import qualified Data.Time as T
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck
import Wasp.Cli.Command.News (NewsReport (..), getMandatoryNewsReport, getMandatoryNewsReportForExistingUser)
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

arbitraryUTCTime :: Gen T.UTCTime
arbitraryUTCTime = T.UTCTime <$> arbitraryDay <*> pure 0
  where
    arbitraryDay = T.fromGregorian <$> choose (2020, 2025) <*> choose (1, 12) <*> choose (1, 28)

spec_getMandatoryNewsReport :: Spec
spec_getMandatoryNewsReport = do
  describe "getMandatoryNewsReport" $ do
    it "shows nothing and marks all as seen for first time users" $ do
      let firstTimeUserInfo = LocalNewsInfo {lastReportAt = Nothing, seenNewsIds = Set.empty}
          newsEntries =
            [ NewsEntry {id = "1", title = "News 1", body = "Body 1", level = High, publishedAt = someTime},
              NewsEntry {id = "2", title = "News 2", body = "Body 2", level = Moderate, publishedAt = someTime}
            ]
          report = getMandatoryNewsReport firstTimeUserInfo newsEntries
      report.newsToShow `shouldBe` []
      report.requireConfirmation `shouldBe` False
      report.newsToConsiderSeen `shouldBe` newsEntries

    prop "delegates to getMandatoryNewsReportForExistingUser for existing users" $
      \localNewsInfo newsEntries ->
        not (isFirstTimeUser localNewsInfo) ==>
          getMandatoryNewsReport localNewsInfo newsEntries
            == getMandatoryNewsReportForExistingUser localNewsInfo newsEntries
  where
    someTime = T.UTCTime (T.fromGregorian 2024 1 1) 0
    isFirstTimeUser info = isNothing info.lastReportAt

spec_getMandatoryNewsReportForExistingUser :: Spec
spec_getMandatoryNewsReportForExistingUser = do
  describe "getMandatoryNewsReportForExistingUser" $ do
    prop "only shows news that are at least moderate level" $
      testReportProperty $ \_ _ report ->
        all ((>= Moderate) . (.level)) report.newsToShow

    prop "does not show news that were previously seen" $
      testReportProperty $ \localNewsInfo _ report ->
        not $ any ((`Set.member` localNewsInfo.seenNewsIds) . (.id)) report.newsToShow

    prop "requires confirmation if and only if high priority news is shown" $
      testReportProperty $ \_ _ report ->
        report.requireConfirmation == any ((== High) . (.level)) report.newsToShow

    prop "marks all shown news as seen when confirmation is required" $
      testReportProperty $ \_ _ report ->
        not report.requireConfirmation || report.newsToConsiderSeen == report.newsToShow

    prop "does not mark any news as seen when confirmation is not required" $
      testReportProperty $ \_ _ report ->
        report.requireConfirmation || null report.newsToConsiderSeen

    -- TODO: This test is basically the implementation
    prop "shows all relevant unseen news" $
      testReportProperty $ \localNewsInfo newsEntries report ->
        let relevantUnseenNews = filter isRelevant . filter isUnseen $ newsEntries
            isRelevant = (>= Moderate) . (.level)
            isUnseen entry = entry.id `Set.notMember` localNewsInfo.seenNewsIds
         in all (`elem` report.newsToShow) relevantUnseenNews
  where
    testReportProperty assertProperty localNewsInfoInput newsEntriesInput =
      let report = getMandatoryNewsReportForExistingUser localNewsInfoInput newsEntriesInput
       in assertProperty localNewsInfoInput newsEntriesInput report
