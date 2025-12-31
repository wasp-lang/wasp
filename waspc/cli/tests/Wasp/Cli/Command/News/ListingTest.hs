module Wasp.Cli.Command.News.ListingTest where

import Data.List (subsequences)
import qualified Data.Time as T
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (Gen, elements, forAll, (===), (==>))
import Wasp.Cli.Command.News.Core (NewsEntry (..), NewsLevel (..))
import Wasp.Cli.Command.News.Listing
  ( NewsListing (..),
    getNewsFromListing,
    getNewsToMarkAsSeen,
    getNewsToShow,
    isConfirmationRequired,
  )
import Wasp.Cli.Command.News.LocalNewsState
  ( LocalNewsState,
    emptyLocalNewsState,
    markNewsAsSeen,
  )

spec_getNewsToShow :: Spec
spec_getNewsToShow = do
  describe "getNewsToShow" $ do
    describe "for UserRequestedAllNews" $ do
      it "returns all news in the listing" $ do
        let newsState = markNewsAsSeen allNews emptyLocalNewsState
        getNewsToShow newsState (UserRequestedAllNews allNews) `shouldBe` allNews

    describe "for WaspRequestedMustSeeNews" $ do
      it "returns unseen news that are at least important" $ do
        let newsState = markNewsAsSeen [info1, important1, critical1] emptyLocalNewsState
        getNewsToShow newsState (WaspRequestedMustSeeNews [info1, important1, important2, critical1, critical2])
          `shouldMatchList` [important2, critical2]

spec_isConfirmationRequired :: Spec
spec_isConfirmationRequired = do
  describe "isConfirmationRequired" $ do
    describe "for UserRequestedAllNews" $ do
      it "returns False" $ do
        let newsState = markNewsAsSeen allNews emptyLocalNewsState
        isConfirmationRequired newsState (UserRequestedAllNews allNews) `shouldBe` False

    -- TODO: bad test, too similar to implementation
    describe "for WaspRequestedMustSeeNews" $ do
      prop "returns True if and only if there is previous news history and newsToShow includes critical news" $
        forAll waspInvokedScenario $ \(newsState, listing) ->
          isConfirmationRequired newsState listing
            === (newsState /= emptyLocalNewsState && any ((== Critical) . level) (getNewsToShow newsState listing))

spec_getNewsToMarkAsSeen :: Spec
spec_getNewsToMarkAsSeen = do
  describe "getNewsToMarkAsSeen" $ do
    describe "for UserRequestedAllNews" $ do
      it "returns all news entries" $ do
        let newsState = markNewsAsSeen allNews emptyLocalNewsState
        getNewsToMarkAsSeen newsState (UserRequestedAllNews allNews) `shouldMatchList` allNews

    describe "for WaspRequestedMustSeeNews" $ do
      prop "returns all news when there is no previous news history" $
        forAll waspInvokedScenario $ \(_, listing) ->
          getNewsToMarkAsSeen emptyLocalNewsState listing === getNewsFromListing listing

      prop "returns an empty list when there is previous news history and confirmation is not required" $
        forAll waspInvokedScenario $ \(newsState, listing) ->
          not (isConfirmationRequired newsState listing) && newsState /= emptyLocalNewsState ==>
            getNewsToMarkAsSeen newsState listing === []

      prop "returns shown news when confirmation is required" $
        forAll waspInvokedScenario $ \(newsState, listing) ->
          isConfirmationRequired newsState listing ==>
            getNewsToMarkAsSeen newsState listing === getNewsToShow newsState listing

waspInvokedScenario :: Gen (LocalNewsState, NewsListing)
waspInvokedScenario = do
  seenNews <- subsetOf allNews
  newsInListing <- subsetOf allNews
  let newsState = markNewsAsSeen seenNews emptyLocalNewsState
      listing = WaspRequestedMustSeeNews newsInListing
  return (newsState, listing)

subsetOf :: [a] -> Gen [a]
subsetOf = elements . subsequences

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
