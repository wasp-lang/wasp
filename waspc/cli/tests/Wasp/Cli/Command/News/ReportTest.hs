{-# LANGUAGE LambdaCase #-}
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
  ( NewsAction (..),
    makeUserInvokedNewsAction,
    makeWaspInvokedNewsAction,
  )

spec_makeUserInvokedNewsAction :: Spec
spec_makeUserInvokedNewsAction = do
  describe "makeUserInvokedNewsAction" $ do
    prop "creates ShowAllAndMarkSeen with all news entries" $
      \newsEntries ->
        makeUserInvokedNewsAction newsEntries == ShowAllAndMarkSeen newsEntries

spec_makeWaspInvokedNewsAction :: Spec
spec_makeWaspInvokedNewsAction = do
  describe "makeWaspInvokedNewsAction" $ do
    describe "for first time users" $ do
      prop "creates MarkSeenWithoutShowing with all news" $
        \newsEntries ->
          makeWaspInvokedNewsAction emptyLocalNewsState newsEntries
            == MarkSeenWithoutShowing newsEntries

    describe "for existing users" $ do
      prop "only includes news that are at least important" $
        forExistingUser $ \localNewsState newsEntries ->
          let news = getNewsFromAction $ makeWaspInvokedNewsAction localNewsState newsEntries
           in all ((>= Important) . level) news

      prop "does not include news that were previously seen" $
        forExistingUser $ \localNewsState newsEntries ->
          let news = getNewsFromAction $ makeWaspInvokedNewsAction localNewsState newsEntries
           in not $ any (wasNewsEntrySeen localNewsState) news

      prop "creates ShowWithConfirmation iff at least one critical news entry exists" $
        forExistingUser $ \localNewsState newsEntries ->
          let action = makeWaspInvokedNewsAction localNewsState newsEntries
              news = getNewsFromAction action
              hasCritical = any ((== Critical) . level) news
           in hasCritical == isShowWithConfirmation action

      it "includes all unseen important+ news" $ do
        let seenNewsEntry = NewsEntry "seen-1" "Seen Critical" "" Critical someTime
            unseenNewsEntries =
              [ NewsEntry "unseen-1" "Unseen Critical" "" Critical someTime,
                NewsEntry "unseen-2" "Unseen Important" "" Important someTime,
                NewsEntry "unseen-3" "Unseen Info" "" Info someTime
              ]
            newsState = markNewsAsSeen [seenNewsEntry] emptyLocalNewsState
            allNewsEntries = seenNewsEntry : unseenNewsEntries
            action = makeWaspInvokedNewsAction newsState allNewsEntries

        map (.id) (getNewsFromAction action) `shouldMatchList` ["unseen-1", "unseen-2"]
  where
    someTime = T.UTCTime (T.fromGregorian 2024 1 1) 0
    forExistingUser prop' localNewsState newsEntries =
      not (localNewsState == emptyLocalNewsState) ==> prop' localNewsState newsEntries

getNewsFromAction :: NewsAction -> [NewsEntry]
getNewsFromAction = \case
  ShowAllAndMarkSeen news -> news
  MarkSeenWithoutShowing news -> news
  ShowWithConfirmation news -> news
  ShowWithoutMarkingSeen news -> news

isShowWithConfirmation :: NewsAction -> Bool
isShowWithConfirmation (ShowWithConfirmation _) = True
isShowWithConfirmation _ = False

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
