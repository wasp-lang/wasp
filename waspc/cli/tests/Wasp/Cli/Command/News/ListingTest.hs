{-# LANGUAGE OverloadedRecordDot #-}

module Wasp.Cli.Command.News.ListingTest where

import qualified Data.Time as T
import Test.Hspec
import Wasp.Cli.Command.News.Core (NewsEntry (..), NewsLevel (..))
import Wasp.Cli.Command.News.Listing
  ( NewsListing (..),
    getNewsToMarkAsSeen,
    getNewsToShow,
    isConfirmationRequired,
  )
import Wasp.Cli.Command.News.LocalNewsState
  ( emptyLocalNewsState,
    markNewsAsSeen,
  )

spec_getNewsToShow :: Spec
spec_getNewsToShow = do
  describe "getNewsToShow" $ do
    describe "for UserRequestedAllNews" $ do
      it "returns all news entries including seen ones" $ do
        let newsState = markNewsAsSeen [info1] emptyLocalNewsState
            newsToShow = getNewsToShow newsState (UserRequestedAllNews allNews)
        newsToShow `shouldMatchList` allNews

    describe "for WaspRequestedMustSeeNews" $ do
      it "returns an empty list when there is no previous news history" $ do
        getNewsToShow emptyLocalNewsState (WaspRequestedMustSeeNews allNews) `shouldBe` []

      it "returns unseen news that are important or higher when there is previous news history" $ do
        let newsState = markNewsAsSeen [critical1, important1] emptyLocalNewsState
            newsToShow = getNewsToShow newsState (WaspRequestedMustSeeNews allNews)
        newsToShow `shouldMatchList` [important2, critical2]

spec_isConfirmationRequired :: Spec
spec_isConfirmationRequired = do
  describe "isConfirmationRequired" $ do
    it "always returns False for UserRequestedAllNews with critical news" $ do
      let newsState = markNewsAsSeen [info1] emptyLocalNewsState
      isConfirmationRequired newsState (UserRequestedAllNews allNews)
        `shouldBe` False

    describe "for WaspRequestedMustSeeNews" $ do
      it "returns True when unseen news include critical news" $ do
        let newsState = markNewsAsSeen [info1] emptyLocalNewsState
        isConfirmationRequired newsState (WaspRequestedMustSeeNews allNews)
          `shouldBe` True

      it "returns False when unseen news don't include critical news" $ do
        let newsState = markNewsAsSeen [critical1, critical2] emptyLocalNewsState
        isConfirmationRequired newsState (WaspRequestedMustSeeNews allNews)
          `shouldBe` False

spec_getNewsMarkedAsSeen :: Spec
spec_getNewsMarkedAsSeen = do
  describe "getNewsToMarkAsSeen" $ do
    it "returns all news entries for UserRequestedAllNews" $ do
      let newsState = markNewsAsSeen [info1] emptyLocalNewsState
          toMark = getNewsToMarkAsSeen newsState (UserRequestedAllNews allNews)
      toMark `shouldMatchList` allNews

    describe "for WaspRequestedMustSeeNews" $ do
      it "returns all news entries when there is no news history" $ do
        let toMark = getNewsToMarkAsSeen emptyLocalNewsState (WaspRequestedMustSeeNews allNews)
        toMark `shouldMatchList` allNews

      it "returns unseen important+ news when there are unseen critical news and there is news history" $ do
        let newsState = markNewsAsSeen [important2, info2, critical2] emptyLocalNewsState
            toMark = getNewsToMarkAsSeen newsState (WaspRequestedMustSeeNews allNews)
        toMark `shouldMatchList` [important1, critical1]

      it "returns an empty list when there are no unseen critical news and there is news history" $ do
        let newsState = markNewsAsSeen [info1] emptyLocalNewsState
            toMark = getNewsToMarkAsSeen newsState (WaspRequestedMustSeeNews [important1, info2])
        toMark `shouldBe` []

allNews :: [NewsEntry]
allNews = [info1, info2, important1, important2, critical1, critical2]

someTime :: T.UTCTime
someTime = T.UTCTime (T.fromGregorian 2024 1 1) 0

critical1, critical2 :: NewsEntry
critical1 = NewsEntry "critical-1" "Critical 1" "" Critical someTime
critical2 = NewsEntry "critical-2" "Critical 2" "" Critical someTime

important1, important2 :: NewsEntry
important1 = NewsEntry "important-1" "Important 1" "" Important someTime
important2 = NewsEntry "important-2" "Important 2" "" Important someTime

info1, info2 :: NewsEntry
info1 = NewsEntry "info-1" "Info 1" "" Info someTime
info2 = NewsEntry "info-2" "Info 2" "" Info someTime
