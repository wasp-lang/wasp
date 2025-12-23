{-# LANGUAGE OverloadedRecordDot #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Wasp.Cli.Command.News.ReportTest where

import qualified Data.Set as Set
import qualified Data.Time as T
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck
import Test.QuickCheck.Arbitrary.Generic (genericArbitrary)
import Wasp.Cli.Command.News.Core (NewsEntry (..), NewsLevel (..))
import Wasp.Cli.Command.News.Persistence (LocalNewsState (..), emptyLocalNewsState)
import Wasp.Cli.Command.News.Report
  ( NewsReport (..),
    makeMandatoryNewsReport,
    makeMandatoryNewsReportForExistingUser,
    makeVoluntaryNewsReport,
  )

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

spec_makeMandatoryNewsReport :: Spec
spec_makeMandatoryNewsReport = do
  describe "makeMandatoryNewsReport" $ do
    prop "shows nothing and marks all as seen for first time users" $
      \newsEntries ->
        makeMandatoryNewsReport emptyLocalNewsState newsEntries
          == NewsReport
            { newsToShow = [],
              requireConfirmation = False,
              newsToConsiderSeen = newsEntries
            }

    prop "delegates to makeMandatoryNewsReportForExistingUser for existing users" $
      \localNewsState newsEntries ->
        not (isFirstTimeUser localNewsState) ==>
          makeMandatoryNewsReport localNewsState newsEntries
            == makeMandatoryNewsReportForExistingUser localNewsState newsEntries
  where
    isFirstTimeUser state = state == emptyLocalNewsState

spec_makeMandatoryNewsReportForExistingUser :: Spec
spec_makeMandatoryNewsReportForExistingUser = do
  describe "makeMandatoryNewsReportForExistingUser" $ do
    prop "only shows news that are at least important" $
      testReportProperty $ \_ _ report ->
        all ((>= Important) . level) report.newsToShow

    prop "does not show news that were previously seen" $
      testReportProperty $ \localNewsState _ report ->
        not $ any ((`Set.member` localNewsState.seenNewsIds) . (.id)) report.newsToShow

    prop "requires confirmation if and only if at least one critical priority news entry is shown" $
      testReportProperty $ \_ _ report ->
        report.requireConfirmation == any ((== Critical) . level) report.newsToShow

    prop "marks all shown news as seen when confirmation is required" $
      testReportProperty $ \_ _ report ->
        not report.requireConfirmation || report.newsToConsiderSeen == report.newsToShow

    prop "does not mark any news as seen when confirmation is not required" $
      testReportProperty $ \_ _ report ->
        report.requireConfirmation || null report.newsToConsiderSeen

    it "shows all unseen important+ news" $ do
      let state = LocalNewsState (Just someTime) (Set.singleton "seen-1")
          newsEntries =
            [ NewsEntry "seen-1" "Seen Critical" "" Critical someTime,
              NewsEntry "unseen-1" "Unseen Critical" "" Critical someTime,
              NewsEntry "unseen-2" "Unseen Important" "" Important someTime,
              NewsEntry "unseen-3" "Unseen Info" "" Info someTime
            ]
          report = makeMandatoryNewsReportForExistingUser state newsEntries
      map (.id) report.newsToShow `shouldMatchList` ["unseen-1", "unseen-2"]
  where
    someTime = T.UTCTime (T.fromGregorian 2024 1 1) 0
    testReportProperty assertProperty localNewsStateInput newsEntriesInput =
      let report = makeMandatoryNewsReportForExistingUser localNewsStateInput newsEntriesInput
       in assertProperty localNewsStateInput newsEntriesInput report

instance Arbitrary NewsLevel where
  arbitrary = genericArbitrary

instance Arbitrary NewsEntry where
  arbitrary = genericArbitrary

instance Arbitrary LocalNewsState where
  arbitrary = genericArbitrary

instance Arbitrary T.UTCTime where
  arbitrary = T.UTCTime <$> arbitraryDay <*> arbitraryDiffTime
    where
      arbitraryDay = T.fromGregorian <$> choose (2010, 2020) <*> choose (1, 12) <*> choose (1, 28)
      arbitraryDiffTime = T.secondsToDiffTime <$> choose (0, 86399)
