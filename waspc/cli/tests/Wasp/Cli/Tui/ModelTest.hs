module Wasp.Cli.Tui.ModelTest where

import Data.List (foldl')
import qualified Data.Text as T
import Data.Time.Calendar (fromGregorian)
import Data.Time.Clock (UTCTime (..), secondsToDiffTime)
import Test.Hspec
import Wasp.Cli.Tui.Ansi (segmentsToText)
import Wasp.Cli.Tui.Layout (LogRow (..), visibleLogRows)
import Wasp.Cli.Tui.Model
  ( ChromeLayout (..),
    TuiEvent (..),
    TuiState,
    activePane,
    applyTuiEvent,
    chromeLayoutFor,
    logBodyHeightFor,
    makeTuiState,
    selectPaneByIndex,
    setTerminalSize,
    toggleActivePaneFollow,
  )
import Wasp.Cli.Tui.Pane (Pane (..))

-- | A state whose active tab is the Wasp one, so that 'TuiWaspOutput' lands on
-- the tab we are looking at. The terminal is small on purpose: the log body
-- ends up 3 rows tall, which is what makes scrolling observable.
stateWithWaspTabActive :: TuiState
stateWithWaspTabActive =
  setTerminalSize terminalWidth terminalHeight
    $ selectPaneByIndex 0
    $ makeTuiState "testapp" [] someTime
  where
    terminalWidth = 40
    -- Five rows of chrome above the log body and two below.
    terminalHeight = 3 + 5 + 2
    someTime = UTCTime (fromGregorian 2026 10 11) (secondsToDiffTime 0)

feedLines :: [T.Text] -> TuiState -> TuiState
feedLines texts state = foldl' feedLine state texts
  where
    feedLine acc text = applyTuiEvent (TuiWaspOutput (text <> "\n")) acc

visibleTexts :: TuiState -> [T.Text]
visibleTexts state =
  map (segmentsToText . logRowSegments)
    $ fst
    $ visibleLogRows 38 3 (activePane state)

spec_pausedPaneHoldsItsPlace :: Spec
spec_pausedPaneHoldsItsPlace = do
  describe "a paused tab while output keeps arriving" $ do
    let afterFirstLines = feedLines ["one", "two", "three"] stateWithWaspTabActive

    it "follows the latest output by default" $ do
      visibleTexts afterFirstLines `shouldBe` ["one", "two", "three"]
      visibleTexts (feedLines ["four"] afterFirstLines) `shouldBe` ["two", "three", "four"]

    it "stops moving once paused" $ do
      let paused = toggleActivePaneFollow afterFirstLines
      paneIsFollowing (activePane paused) `shouldBe` False
      visibleTexts paused `shouldBe` ["one", "two", "three"]

    -- The whole point of pausing: you saw something worth reading, and the
    -- server is still talking.
    it "keeps showing the same lines as new ones arrive" $ do
      let paused = toggleActivePaneFollow afterFirstLines
      visibleTexts (feedLines ["four", "five", "six"] paused)
        `shouldBe` ["one", "two", "three"]

    it "counts rows, not lines, so wrapped output doesn't push the view" $ do
      let paused = toggleActivePaneFollow afterFirstLines
          -- Wider than the 38-column body, so it takes two rows.
          longLine = T.replicate 50 "x"
      visibleTexts (feedLines [longLine] paused) `shouldBe` ["one", "two", "three"]

    it "jumps back to the latest output when unpaused" $ do
      let paused = toggleActivePaneFollow afterFirstLines
          caughtUp = toggleActivePaneFollow (feedLines ["four", "five"] paused)
      paneIsFollowing (activePane caughtUp) `shouldBe` True
      visibleTexts caughtUp `shouldBe` ["three", "four", "five"]

spec_chromeFitsShortTerminals :: Spec
spec_chromeFitsShortTerminals = do
  describe "chrome in a short terminal" $ do
    -- The logs are the point, so in a split pane the chrome gives way rather
    -- than crowding them out.
    it "keeps everything when there is room" $ do
      let chrome = chromeLayoutFor 40
      (chromeShowsUrls chrome, chromeShowsSpacer chrome) `shouldBe` (True, True)
      logBodyHeightFor 40 `shouldBe` 33

    it "drops the spacer first" $ do
      let chrome = chromeLayoutFor 16
      (chromeShowsUrls chrome, chromeShowsSpacer chrome) `shouldBe` (True, False)
      logBodyHeightFor 16 `shouldBe` 10

    it "drops the URL row too when very short" $ do
      let chrome = chromeLayoutFor 8
      (chromeShowsUrls chrome, chromeShowsSpacer chrome) `shouldBe` (False, False)
      logBodyHeightFor 8 `shouldBe` 3

    it "always leaves at least one row of log" $ do
      map logBodyHeightFor [0, 1, 4, 6] `shouldBe` [1, 1, 1, 1]
