{-# LANGUAGE OverloadedRecordDot #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Wasp.Cli.Command.News.ListingTest where

import qualified Data.Time as T
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck
import Test.QuickCheck.Arbitrary.Generic (genericArbitrary)
import Wasp.Cli.Command.News.Listing
  ( NewsListing (..),
    getNewsToMarkAsSeen,
    getNewsToShow,
    isConfirmationRequired,
  )
import Wasp.Cli.Command.News.Core (NewsEntry (..), NewsLevel (..))
import Wasp.Cli.Command.News.LocalNewsState
  ( LocalNewsState,
    emptyLocalNewsState,
    markNewsAsSeen,
    wasNewsEntrySeen,
  )

spec_getNewsToShow :: Spec
spec_getNewsToShow = do
  describe "getNewsToShow" $ do
    describe "for UserRequestedAllNews" $ do
      prop "returns all news entries" $
        \localNewsState newsEntries ->
          getNewsToShow localNewsState (UserRequestedAllNews newsEntries) == newsEntries

    describe "for WaspRequestedMustSeeNews" $ do
      describe "for first time users" $ do
        prop "returns empty list" $
          \newsEntries ->
            null (getNewsToShow emptyLocalNewsState (WaspRequestedMustSeeNews newsEntries))

      describe "for existing users" $ do
        prop "only includes news that are at least important" $
          forExistingUser $ \localNewsState newsEntries ->
            let news = getNewsToShow localNewsState (WaspRequestedMustSeeNews newsEntries)
             in all ((>= Important) . level) news

        prop "does not include news that were previously seen" $
          forExistingUser $ \localNewsState newsEntries ->
            let news = getNewsToShow localNewsState (WaspRequestedMustSeeNews newsEntries)
             in not $ any (wasNewsEntrySeen localNewsState) news

        it "includes all unseen important+ news" $ do
          let seenNewsEntry = NewsEntry "seen-1" "Seen Critical" "" Critical someTime
              unseenNewsEntries =
                [ NewsEntry "unseen-1" "Unseen Critical" "" Critical someTime,
                  NewsEntry "unseen-2" "Unseen Important" "" Important someTime,
                  NewsEntry "unseen-3" "Unseen Info" "" Info someTime
                ]
              newsState = markNewsAsSeen [seenNewsEntry] emptyLocalNewsState
              allNewsEntries = seenNewsEntry : unseenNewsEntries
              news = getNewsToShow newsState (WaspRequestedMustSeeNews allNewsEntries)

          map (.id) news `shouldMatchList` ["unseen-1", "unseen-2"]
  where
    someTime = T.UTCTime (T.fromGregorian 2024 1 1) 0

spec_isConfirmationRequired :: Spec
spec_isConfirmationRequired = do
  describe "isConfirmationRequired" $ do
    describe "for UserRequestedAllNews" $ do
      prop "returns False" $
        \localNewsState newsEntries ->
          not (isConfirmationRequired localNewsState (UserRequestedAllNews newsEntries))

    describe "for WaspRequestedMustSeeNews" $ do
      prop "returns True iff at least one critical news entry would be shown" $
        forExistingUser $ \localNewsState newsEntries ->
          let listing = WaspRequestedMustSeeNews newsEntries
              news = getNewsToShow localNewsState listing
              hasCritical = any ((== Critical) . level) news
           in hasCritical == isConfirmationRequired localNewsState listing

spec_getNewsMarkedAsSeen :: Spec
spec_getNewsMarkedAsSeen = do
  describe "getNewsMarkedAsSeen" $ do
    describe "for UserRequestedAllNews" $ do
      prop "returns all news entries" $
        \localNewsState newsEntries ->
          getNewsToMarkAsSeen localNewsState (UserRequestedAllNews newsEntries) == newsEntries

    describe "for WaspRequestedMustSeeNews" $ do
      describe "for first time users" $ do
        prop "returns all news entries" $
          \newsEntries ->
            getNewsToMarkAsSeen emptyLocalNewsState (WaspRequestedMustSeeNews newsEntries) == newsEntries

      describe "for existing users with critical news" $ do
        it "returns news to show (if user confirms)" $ do
          let dummySeenEntry = NewsEntry "seen-1" "Seen" "" Info someTime
              newsEntries =
                [ NewsEntry "critical-1" "Critical" "" Critical someTime,
                  NewsEntry "info-1" "Info" "" Info someTime
                ]
              newsState = markNewsAsSeen [dummySeenEntry] emptyLocalNewsState
              listing = WaspRequestedMustSeeNews newsEntries
              toMark = getNewsToMarkAsSeen newsState listing

          map (.id) toMark `shouldMatchList` ["critical-1"]

      describe "for existing users with no critical news" $ do
        it "returns empty list" $ do
          let dummySeenEntry = NewsEntry "seen-1" "Seen" "" Info someTime
              newsEntries =
                [ NewsEntry "important-1" "Important" "" Important someTime,
                  NewsEntry "info-1" "Info" "" Info someTime
                ]
              newsState = markNewsAsSeen [dummySeenEntry] emptyLocalNewsState
              listing = WaspRequestedMustSeeNews newsEntries
              toMark = getNewsToMarkAsSeen newsState listing

          toMark `shouldBe` []
  where
    someTime = T.UTCTime (T.fromGregorian 2024 1 1) 0

forExistingUser ::
  (Testable prop) =>
  (LocalNewsState -> [NewsEntry] -> prop) ->
  LocalNewsState ->
  [NewsEntry] ->
  Property
forExistingUser prop' localNewsState newsEntries =
  (localNewsState /= emptyLocalNewsState) ==> prop' localNewsState newsEntries

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
