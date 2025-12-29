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
    makeWaspInvokedNewsActionForExistingUser,
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
    prop "creates MarkSeenWithoutShowing for first time users" $
      \newsEntries ->
        makeWaspInvokedNewsAction emptyLocalNewsState newsEntries
          == MarkSeenWithoutShowing newsEntries

    prop "delegates to makeWaspInvokedNewsActionForExistingUser for existing users" $
      \localNewsState newsEntries ->
        not (isFirstTimeUser localNewsState) ==>
          makeWaspInvokedNewsAction localNewsState newsEntries
            == makeWaspInvokedNewsActionForExistingUser localNewsState newsEntries
  where
    isFirstTimeUser state = state == emptyLocalNewsState

spec_makeWaspInvokedNewsActionForExistingUser :: Spec
spec_makeWaspInvokedNewsActionForExistingUser = do
  describe "makeWaspInvokedNewsActionForExistingUser" $ do
    prop "only includes news that are at least important" $
      testActionProperty $ \_ _ news ->
        all ((>= Important) . level) news

    prop "does not include news that were previously seen" $
      testActionProperty $ \localNewsState _ news ->
        not $ any (wasNewsEntrySeen localNewsState) news

    prop "creates ShowWithConfirmation if and only if at least one critical news entry exists" $
      \localNewsState newsEntries ->
        let action = makeWaspInvokedNewsActionForExistingUser localNewsState newsEntries
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
          action = makeWaspInvokedNewsActionForExistingUser newsState allNewsEntries

      map (.id) (getNewsFromAction action) `shouldMatchList` ["unseen-1", "unseen-2"]
  where
    someTime = T.UTCTime (T.fromGregorian 2024 1 1) 0
    testActionProperty assertProperty localNewsStateInput newsEntriesInput =
      let action = makeWaspInvokedNewsActionForExistingUser localNewsStateInput newsEntriesInput
          news = getNewsFromAction action
       in assertProperty localNewsStateInput newsEntriesInput news

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
