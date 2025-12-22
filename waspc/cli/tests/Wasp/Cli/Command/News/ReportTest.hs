{-# LANGUAGE OverloadedRecordDot #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Wasp.Cli.Command.News.ReportTest where

import qualified Data.Set as Set
import qualified Data.Time as T
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck
import Wasp.Cli.Command.News.Common (NewsEntry (..), NewsLevel (..))
import Wasp.Cli.Command.News.Persistence (LocalNewsState (..), emptyLocalNewsState)
import Wasp.Cli.Command.News.Report
  ( NewsReport (..),
    makeMandatoryNewsReport,
    makeMandatoryNewsReportForExistingUser,
    makeVoluntaryNewsReport,
  )

spec_makeMandatoryNewsReport :: Spec
spec_makeMandatoryNewsReport = do
  describe "makeMandatoryNewsReport" $ do
    it "shows nothing and marks all as seen for first time users" $ do
      let newsEntries =
            [ NewsEntry {id = "1", title = "News 1", body = "Body 1", level = High, publishedAt = someTime},
              NewsEntry {id = "2", title = "News 2", body = "Body 2", level = Moderate, publishedAt = someTime}
            ]

      makeMandatoryNewsReport emptyLocalNewsState newsEntries
        `shouldBe` NewsReport
          { newsToShow = [],
            requireConfirmation = False,
            newsToConsiderSeen = newsEntries
          }

    prop "delegates to makeMandatoryNewsReportForExistingUser for existing users" $
      \localNewsState newsEntries ->
        not (isFirstTimeUser localNewsState)
          ==> makeMandatoryNewsReport localNewsState newsEntries
          == makeMandatoryNewsReportForExistingUser localNewsState newsEntries
  where
    someTime = T.UTCTime (T.fromGregorian 2024 1 1) 0
    isFirstTimeUser state = state == emptyLocalNewsState

spec_makeMandatoryNewsReportForExistingUser :: Spec
spec_makeMandatoryNewsReportForExistingUser = do
  describe "makeMandatoryNewsReportForExistingUser" $ do
    prop "only shows news that are at least moderate" $
      testReportProperty $ \_ _ report ->
        all ((>= Moderate) . level) report.newsToShow

    prop "does not show news that were previously seen" $
      testReportProperty $ \localNewsState _ report ->
        not $ any ((`Set.member` localNewsState.seenNewsIds) . (.id)) report.newsToShow

    prop "requires confirmation if and only if at least one high priority news entry is shown" $
      testReportProperty $ \_ _ report ->
        report.requireConfirmation == any ((== High) . level) report.newsToShow

    prop "marks all shown news as seen when confirmation is required" $
      testReportProperty $ \_ _ report ->
        not report.requireConfirmation || report.newsToConsiderSeen == report.newsToShow

    prop "does not mark any news as seen when confirmation is not required" $
      testReportProperty $ \_ _ report ->
        report.requireConfirmation || null report.newsToConsiderSeen

    -- TODO: This test is basically the implementation
    prop "shows all relevant unseen news" $
      testReportProperty $ \localNewsState newsEntries report ->
        let relevantUnseenNews = filter isRelevant . filter isUnseen $ newsEntries
            isRelevant = (>= Moderate) . level
            isUnseen entry = entry.id `Set.notMember` localNewsState.seenNewsIds
         in all (`elem` report.newsToShow) relevantUnseenNews
  where
    testReportProperty assertProperty localNewsStateInput newsEntriesInput =
      let report = makeMandatoryNewsReportForExistingUser localNewsStateInput newsEntriesInput
       in assertProperty localNewsStateInput newsEntriesInput report

spec_makeVoluntaryNewsReport :: Spec
spec_makeVoluntaryNewsReport = do
  describe "makeVoluntaryNewsReport" $ do
    prop "shows all news entries" $
      testReportProperty $ \_ newsEntries report ->
        report.newsToShow == newsEntries

    prop "marks all news entries as seen" $
      testReportProperty $ \_ newsEntries report ->
        report.newsToConsiderSeen == newsEntries

    prop "never requires confirmation" $
      testReportProperty $ \_ _ report ->
        not report.requireConfirmation
  where
    testReportProperty assertProperty localNewsStateInput newsEntriesInput =
      let report = makeVoluntaryNewsReport localNewsStateInput newsEntriesInput
       in assertProperty localNewsStateInput newsEntriesInput report

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

instance Arbitrary LocalNewsState where
  arbitrary =
    (LocalNewsState . Just <$> arbitraryUTCTime)
      <*> (Set.fromList <$> arbitrary)

arbitraryUTCTime :: Gen T.UTCTime
arbitraryUTCTime = T.UTCTime <$> arbitraryDay <*> pure 0
  where
    arbitraryDay = T.fromGregorian <$> choose (2020, 2025) <*> choose (1, 12) <*> choose (1, 28)
