module Wasp.Cli.Tui.LayoutTest where

import Data.List (foldl')
import qualified Data.Text as T
import qualified Graphics.Vty as V
import Test.Hspec
import Wasp.Cli.Tui.Ansi (Segment (..), segmentsToText)
import Wasp.Cli.Tui.Layout (LogRow (..), displayWidth, visibleLogRows, wrapLogLine, wrapSegments)
import Wasp.Cli.Tui.LogBuffer (appendChunk, bufferedLines)
import Wasp.Cli.Tui.Pane
  ( Pane (..),
    PaneId (..),
    makePane,
    scrollPaneBy,
    scrollPaneToBottom,
    scrollPaneToTop,
    togglePaneFollow,
  )

-- | Characters vty's width table treats as two columns wide (CJK ideographs).
--
-- Note that vty's built-in table is out of date for emoji: it calls a rocket
-- one column even though terminals draw it as two. Agreeing with vty is what
-- keeps it from cropping our rows, so that is the measure we wrap by.
wideChars :: T.Text
wideChars = "\26085\26412\35486\28450"

segment :: T.Text -> Segment
segment text = Segment {segmentAttr = V.defAttr, segmentText = text}

wrappedTexts :: Int -> [T.Text] -> [T.Text]
wrappedTexts width = map segmentsToText . wrapSegments width . map segment

spec_wrapSegments :: Spec
spec_wrapSegments = do
  describe "wrapSegments" $ do
    it "leaves a line that fits on one row" $ do
      wrappedTexts 10 ["short"] `shouldBe` ["short"]

    it "leaves a line that exactly fills a row" $ do
      wrappedTexts 5 ["abcde"] `shouldBe` ["abcde"]

    it "splits a line that is too long" $ do
      wrappedTexts 3 ["abcdefg"] `shouldBe` ["abc", "def", "g"]

    it "wraps across segment boundaries" $ do
      wrappedTexts 4 ["ab", "cdef", "gh"] `shouldBe` ["abcd", "efgh"]

    it "keeps each piece's own attribute when it splits a segment" $ do
      let colored text color = Segment {segmentAttr = V.defAttr `V.withForeColor` color, segmentText = text}
          rows = wrapSegments 3 [colored "abcdef" V.red]
      map (map segmentAttr) rows
        `shouldBe` [ [V.defAttr `V.withForeColor` V.red],
                     [V.defAttr `V.withForeColor` V.red]
                   ]
      map segmentsToText rows `shouldBe` ["abc", "def"]

    it "gives an empty line a row of its own" $ do
      wrappedTexts 10 [] `shouldBe` [""]
      wrappedTexts 10 [""] `shouldBe` [""]

    it "never produces a row wider than the width" $ do
      let rows = wrapSegments 7 (map segment ["one ", "two ", "three ", "four"])
      all ((<= 7) . T.length . segmentsToText) rows `shouldBe` True

    -- Vty measures the images it draws in terminal columns, so wrapping that
    -- counts characters hands it rows that are drawn wider than the pane and
    -- get cropped, losing text off the right edge.
    it "wraps by terminal columns, not by characters" $ do
      -- Each of these takes two columns, so only three fit in a six-column row.
      displayWidth wideChars `shouldBe` 8
      wrappedTexts 6 [wideChars] `shouldBe` [T.take 3 wideChars, T.drop 3 wideChars]

    it "keeps a wide character whole rather than splitting it across rows" $ do
      -- Five columns hold two of them; a third would straddle the edge.
      wrappedTexts 5 ["\26085\26412\35486"] `shouldBe` ["\26085\26412", "\35486"]

    it "makes progress even when a character is wider than the row" $ do
      wrappedTexts 1 ["\26085\26412"] `shouldBe` ["\26085", "\26412"]

    -- The property that actually matters: vty never gets a row it has to crop.
    it "never hands vty a row wider than the pane" $ do
      let rows = wrapSegments 8 (map segment ["sent ", T.take 2 wideChars, " to ", T.take 3 wideChars, " now"])
      all ((<= 8) . displayWidth . segmentsToText) rows `shouldBe` True
      T.concat (map segmentsToText rows)
        `shouldBe` T.concat ["sent ", T.take 2 wideChars, " to ", T.take 3 wideChars, " now"]

    it "loses no text" $ do
      let pieces = ["alpha ", "beta ", "gamma delta"]
      T.concat (wrappedTexts 4 pieces) `shouldBe` T.concat pieces

-- | A pane holding one line per given text.
paneWithLines :: [T.Text] -> Pane
paneWithLines texts = pane {paneBuffer = buffer}
  where
    pane = makePane PaneServer
    buffer = foldl' append (paneBuffer pane) texts
    append acc text = fst (appendChunk False (text <> "\n") acc)

rowTexts :: ([LogRow], Int) -> [T.Text]
rowTexts = map (segmentsToText . logRowSegments) . fst

spec_visibleLogRows :: Spec
spec_visibleLogRows = do
  describe "visibleLogRows" $ do
    let pane = paneWithLines ["one", "two", "three", "four", "five"]

    it "shows the newest rows when following" $ do
      rowTexts (visibleLogRows 20 3 pane) `shouldBe` ["three", "four", "five"]

    it "shows everything when the pane is taller than the log" $ do
      rowTexts (visibleLogRows 20 10 pane)
        `shouldBe` ["one", "two", "three", "four", "five"]

    it "reports nothing hidden when following" $ do
      snd (visibleLogRows 20 3 pane) `shouldBe` 0

    it "scrolls back by rows" $ do
      let scrolled = scrollPaneBy 2 pane
      rowTexts (visibleLogRows 20 3 scrolled) `shouldBe` ["one", "two", "three"]
      snd (visibleLogRows 20 3 scrolled) `shouldBe` 2

    it "stops at the oldest line instead of scrolling past it" $ do
      let scrolled = scrollPaneBy 100 pane
      rowTexts (visibleLogRows 20 3 scrolled) `shouldBe` ["one", "two", "three"]
      snd (visibleLogRows 20 3 scrolled) `shouldBe` 2

    it "counts wrapped rows, not lines, when scrolling" $ do
      -- "aaaaaa" wraps into three rows at width 2, so scrolling back by one
      -- row shows its last row at the bottom rather than skipping the line.
      let wrappingPane = paneWithLines ["aaaaaa", "b"]
      rowTexts (visibleLogRows 2 2 wrappingPane) `shouldBe` ["aa", "b"]
      rowTexts (visibleLogRows 2 2 (scrollPaneBy 1 wrappingPane)) `shouldBe` ["aa", "aa"]

    it "shows the oldest rows when jumped to the top" $ do
      rowTexts (visibleLogRows 20 2 (scrollPaneToTop pane)) `shouldBe` ["one", "two"]

    it "copes with an empty pane" $ do
      rowTexts (visibleLogRows 20 5 (makePane PaneWasp)) `shouldBe` []

    it "copes with a degenerate width and height" $ do
      length (fst (visibleLogRows 0 0 pane)) `shouldBe` 1

spec_wrapLogLine :: Spec
spec_wrapLogLine = do
  describe "wrapLogLine" $ do
    -- The renderer draws the gutter bar only on a line's first row. Repeating
    -- it on continuation rows puts it inside anything a selection drags
    -- across a wrap, which is how a wrapped URL becomes unusable.
    it "marks only the first row of a line as the line's start" $ do
      let [line] = toListOfLines "a long line that wraps"
      map logRowIsLineStart (wrapLogLine 8 line) `shouldBe` [True, False, False]

    it "marks a line that fits as a line start" $ do
      let [line] = toListOfLines "short"
      map logRowIsLineStart (wrapLogLine 80 line) `shouldBe` [True]
  where
    toListOfLines text =
      foldr (:) [] $ bufferedLines $ fst $ appendChunk False (text <> "\n") (paneBuffer (makePane PaneServer))

spec_paneFollowing :: Spec
spec_paneFollowing = do
  describe "pausing and following" $ do
    let pane = paneWithLines ["one", "two", "three", "four", "five"]

    -- Pausing while already at the latest output is the common case: you see
    -- something go by and want it to stop moving.
    it "pauses a pane that is sitting at the bottom" $ do
      paneIsFollowing (togglePaneFollow pane) `shouldBe` False

    it "keeps showing the same rows once paused at the bottom" $ do
      rowTexts (visibleLogRows 20 2 (togglePaneFollow pane)) `shouldBe` ["four", "five"]

    it "re-attaches when toggled again" $ do
      paneIsFollowing (togglePaneFollow (togglePaneFollow pane)) `shouldBe` True

    it "detaches when scrolled back and re-attaches at the bottom" $ do
      paneIsFollowing (scrollPaneBy 2 pane) `shouldBe` False
      paneIsFollowing (scrollPaneToBottom (scrollPaneBy 2 pane)) `shouldBe` True
      paneIsFollowing (scrollPaneToTop pane) `shouldBe` False
