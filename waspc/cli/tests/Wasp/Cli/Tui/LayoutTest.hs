module Wasp.Cli.Tui.LayoutTest where

import Data.List (foldl')
import qualified Data.Text as T
import qualified Graphics.Vty as V
import Test.Hspec
import Wasp.Cli.Tui.Ansi (Segment (..), segmentsToText)
import Wasp.Cli.Tui.Layout (LogRow (..), visibleLogRows, wrapSegments)
import Wasp.Cli.Tui.LogBuffer (appendChunk)
import Wasp.Cli.Tui.Pane (Pane (..), PaneId (..), makePane, scrollPaneBy, scrollPaneToTop)

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
