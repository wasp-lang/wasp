module Wasp.Cli.Command.News.LocalNewsStateTest where

import qualified Data.Time as T
import Test.Hspec
import Wasp.Cli.Command.News.Core (NewsEntry (..), NewsLevel (..))
import Wasp.Cli.Command.News.LocalNewsState
  ( emptyLocalNewsState,
    markNewsAsSeen,
    wasNewsEntrySeen,
  )

spec_markNewsAsSeen :: Spec
spec_markNewsAsSeen = do
  describe "markNewsAsSeen" $ do
    it "marks news entries as seen" $ do
      let state = markNewsAsSeen [newsEntry1, newsEntry2] emptyLocalNewsState
      wasNewsEntrySeen state newsEntry1 `shouldBe` True
      wasNewsEntrySeen state newsEntry2 `shouldBe` True
      wasNewsEntrySeen state newsEntry3 `shouldBe` False

    it "correctly joins seen entries" $ do
      let state1 = markNewsAsSeen [newsEntry1] emptyLocalNewsState
          state2 = markNewsAsSeen [newsEntry2] state1
      wasNewsEntrySeen state2 newsEntry1 `shouldBe` True
      wasNewsEntrySeen state2 newsEntry2 `shouldBe` True
      wasNewsEntrySeen state newsEntry3 `shouldBe` False
  where
    newsEntry1 = NewsEntry "news-1" "News 1" "" Info someTime
    newsEntry2 = NewsEntry "news-2" "News 2" "" Info someTime
    newsEntry3 = NewsEntry "news-3" "News 3" "" Info someTime

    someTime = T.UTCTime (T.fromGregorian 2025 1 1) 0
