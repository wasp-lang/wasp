{-# LANGUAGE OverloadedRecordDot #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Wasp.Cli.Command.News.ReportTest where

import qualified Data.Time as T
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck
import Test.QuickCheck.Arbitrary.Generic (genericArbitrary)
import Wasp.Cli.Command.News.Core (NewsEntry (..), NewsLevel (..))
import Wasp.Cli.Command.News.LocalNewsState
  ( LocalNewsState,
    emptyLocalNewsState,
    markNewsAsSeen,
    wasNewsEntrySeen,
  )
import Wasp.Cli.Command.News.Report
  ( NewsReport (..),
    NewsReportInitiator (..),
    makeUserInvokedNewsReport,
    makeWaspInvokedNewsReport,
    makeWaspInvokedNewsReportForExistingUser,
  )

spec_makeUserInvokedNewsReport :: Spec
spec_makeUserInvokedNewsReport = do
  describe "makeUserInvokedNewsReport" $ do
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
      let report = makeUserInvokedNewsReport localNewsStateInput newsEntriesInput
       in assertProperty localNewsStateInput newsEntriesInput report

spec_makeWaspInvokedNewsReport :: Spec
spec_makeWaspInvokedNewsReport = do
  describe "makeWaspInvokedNewsReport" $ do
    prop "shows nothing and marks all as seen for first time users" $
      \newsEntries ->
        makeWaspInvokedNewsReport emptyLocalNewsState newsEntries
          == NewsReport
            { newsToShow = [],
              initiator = Wasp,
              requireConfirmation = False,
              newsToConsiderSeen = newsEntries
            }

    prop "delegates to makeWaspInvokedNewsReportForExistingUser for existing users" $
      \localNewsState newsEntries ->
        not (isFirstTimeUser localNewsState) ==>
          makeWaspInvokedNewsReport localNewsState newsEntries
            == makeWaspInvokedNewsReportForExistingUser localNewsState newsEntries
  where
    isFirstTimeUser state = state == emptyLocalNewsState

spec_makeWaspInvokedNewsReportForExistingUser :: Spec
spec_makeWaspInvokedNewsReportForExistingUser = do
  describe "makeWaspInvokedNewsReportForExistingUser" $ do
    prop "only shows news that are at least important" $
      testReportProperty $ \_ _ report ->
        all ((>= Important) . level) report.newsToShow

    prop "does not show news that were previously seen" $
      testReportProperty $ \localNewsState _ report ->
        not $ any (wasNewsEntrySeen localNewsState) report.newsToShow

    prop "requires confirmation if and only if at least one critical priority news entry is shown" $
      testReportProperty $ \_ _ report ->
        report.requireConfirmation == any ((== Critical) . level) report.newsToShow

    prop "marks all shown news as seen" $
      testReportProperty $ \_ _ report ->
        report.newsToShow == report.newsToConsiderSeen

    it "shows all unseen important+ news" $ do
      let seenNewsEntry = NewsEntry "seen-1" "Seen Critical" "" Critical someTime
          unseenNewsEntries =
            [ NewsEntry "unseen-1" "Unseen Critical" "" Critical someTime,
              NewsEntry "unseen-2" "Unseen Important" "" Important someTime,
              NewsEntry "unseen-3" "Unseen Info" "" Info someTime
            ]
          newsState = markNewsAsSeen [seenNewsEntry] emptyLocalNewsState
          allNewsEntries = seenNewsEntry : unseenNewsEntries
          report = makeWaspInvokedNewsReportForExistingUser newsState allNewsEntries

      map (.id) report.newsToShow `shouldMatchList` ["unseen-1", "unseen-2"]
  where
    someTime = T.UTCTime (T.fromGregorian 2024 1 1) 0
    testReportProperty assertProperty localNewsStateInput newsEntriesInput =
      let report = makeWaspInvokedNewsReportForExistingUser localNewsStateInput newsEntriesInput
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
