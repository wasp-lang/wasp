module Wasp.Cli.Command.News.ListingTest where

import Data.List (subsequences)
import qualified Data.Time as T
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (Gen, elements, forAll, (===), (==>))
import Wasp.Cli.Command.News.Core (NewsEntry (..), NewsLevel (..))
import Wasp.Cli.Command.News.Listing
  ( NewsListing (..),
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
    describe "for UserListingAllNews" $ do
      it "returns all news in the listing" $ do
        let newsState = markNewsAsSeen mockAllNews emptyLocalNewsState
        getNewsToShow newsState (UserListingAllNews mockAllNews) `shouldBe` mockAllNews

    describe "for WaspListingMustSeeNews" $ do
      it "returns unseen news that are at least important" $ do
        let newsState = markNewsAsSeen [info1, important1, critical1] emptyLocalNewsState
        getNewsToShow newsState (WaspListingMustSeeNews [info1, important1, important2, critical1, critical2])
          `shouldMatchList` [important2, critical2]

spec_isConfirmationRequired :: Spec
spec_isConfirmationRequired = do
  describe "isConfirmationRequired" $ do
    describe "for UserListingAllNews" $ do
      it "returns False" $ do
        let newsState = markNewsAsSeen mockAllNews emptyLocalNewsState
        isConfirmationRequired newsState (UserListingAllNews mockAllNews) `shouldBe` False

    -- TODO: bad test, too similar to implementation
    describe "for WaspListingMustSeeNews" $ do
      prop "returns True if and only if there is previous news history and newsToShow includes critical news" $
        forAll waspListingScenario $ \(newsState, listing) ->
          isConfirmationRequired newsState listing
            === (newsState /= emptyLocalNewsState && any ((== Critical) . level) (getNewsToShow newsState listing))

spec_getNewsToMarkAsSeen :: Spec
spec_getNewsToMarkAsSeen = do
  describe "getNewsToMarkAsSeen" $ do
    describe "for UserListingAllNews" $ do
      it "returns all news entries" $ do
        let newsState = markNewsAsSeen mockAllNews emptyLocalNewsState
        getNewsToMarkAsSeen newsState (UserListingAllNews mockAllNews) `shouldMatchList` mockAllNews

    describe "for WaspListingMustSeeNews" $ do
      prop "returns all news when there is no previous news history" $
        forAll waspListingScenario $ \(_, listing) ->
          getNewsToMarkAsSeen emptyLocalNewsState listing === allNews listing

      prop "returns an empty list when there is previous news history and confirmation is not required" $
        forAll waspListingScenario $ \(newsState, listing) ->
          not (isConfirmationRequired newsState listing) && newsState /= emptyLocalNewsState ==>
            getNewsToMarkAsSeen newsState listing === []

      prop "returns shown news when confirmation is required" $
        forAll waspListingScenario $ \(newsState, listing) ->
          isConfirmationRequired newsState listing ==>
            getNewsToMarkAsSeen newsState listing === getNewsToShow newsState listing

waspListingScenario :: Gen (LocalNewsState, NewsListing)
waspListingScenario = do
  seenNews <- subsetOf mockAllNews
  newsInListing <- subsetOf mockAllNews
  let newsState = markNewsAsSeen seenNews emptyLocalNewsState
      listing = WaspListingMustSeeNews newsInListing
  return (newsState, listing)

subsetOf :: [a] -> Gen [a]
subsetOf = elements . subsequences

mockAllNews :: [NewsEntry]
mockAllNews = [info1, info2, important1, important2, critical1, critical2]

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
