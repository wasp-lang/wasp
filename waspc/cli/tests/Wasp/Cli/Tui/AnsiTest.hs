module Wasp.Cli.Tui.AnsiTest where

import qualified Data.Text as T
import qualified Graphics.Vty as V
import Test.Hspec
import Wasp.Cli.Tui.Ansi
  ( AnsiState (..),
    Segment (..),
    initialAnsiState,
    parseAnsi,
    segmentsHaveColor,
    segmentsToText,
  )

-- | Parses a chunk that carries nothing over from a previous one, the common case.
parse :: T.Text -> [Segment]
parse = snd . parseAnsi initialAnsiState

spec_parseAnsi :: Spec
spec_parseAnsi = do
  describe "parseAnsi" $ do
    it "leaves plain text alone" $ do
      segmentsToText (parse "hello world") `shouldBe` "hello world"

    it "drops the escape sequences from the text" $ do
      segmentsToText (parse "\ESC[31mred\ESC[0m and plain")
        `shouldBe` "red and plain"

    it "colors the text between a color code and its reset" $ do
      map segmentAttr (parse "\ESC[31mred\ESC[0mplain")
        `shouldBe` [V.defAttr `V.withForeColor` V.ISOColor 1, V.defAttr]

    it "understands bright colors" $ do
      map segmentAttr (parse "\ESC[92mgreen")
        `shouldBe` [V.defAttr `V.withForeColor` V.ISOColor 10]

    it "understands 256-color codes" $ do
      map segmentAttr (parse "\ESC[38;5;200mpink")
        `shouldBe` [V.defAttr `V.withForeColor` V.Color240 184]

    it "understands truecolor codes" $ do
      map segmentAttr (parse "\ESC[38;2;245;200;66mbrand")
        `shouldBe` [V.defAttr `V.withForeColor` V.rgbColor (245 :: Int) 200 66]

    it "applies and removes styles" $ do
      map (V.attrStyle . segmentAttr) (parse "\ESC[1mbold\ESC[22mnormal")
        `shouldBe` [V.SetTo V.bold, V.SetTo 0]

    it "combines several codes in one sequence" $ do
      map segmentAttr (parse "\ESC[1;31mloud")
        `shouldBe` [(V.defAttr `V.withStyle` V.bold) `V.withForeColor` V.ISOColor 1]

    it "carries the attribute over to the next chunk" $ do
      let (stateAfterChunk, _) = parseAnsi initialAnsiState "\ESC[31mred, to be continued"
      map segmentAttr (snd (parseAnsi stateAfterChunk " still red"))
        `shouldBe` [V.defAttr `V.withForeColor` V.ISOColor 1]

    it "ignores escape sequences that are not about styling" $ do
      -- Cursor movement and screen clearing, which we have no use for.
      segmentsToText (parse "\ESC[2J\ESC[1;1Hcleared") `shouldBe` "cleared"

    it "drops OSC sequences along with their payload" $ do
      segmentsToText (parse "\ESC]0;window title\BELafter") `shouldBe` "after"

    it "drops control characters that would corrupt the layout" $ do
      segmentsToText (parse "ab\BSc") `shouldBe` "abc"

    it "keeps tabs, which are expanded later" $ do
      segmentsToText (parse "a\tb") `shouldBe` "a\tb"

    it "holds back an escape sequence that the chunk boundary cut in half" $ do
      let (stateAfterChunk, firstSegments) = parseAnsi initialAnsiState "text\ESC[3"
      segmentsToText firstSegments `shouldBe` "text"
      ansiPendingEscape stateAfterChunk `shouldBe` "\ESC[3"
      -- The rest of the sequence arrives next and colors what follows.
      let (_, secondSegments) = parseAnsi stateAfterChunk "1mred"
      segmentsToText secondSegments `shouldBe` "red"
      map segmentAttr secondSegments `shouldBe` [V.defAttr `V.withForeColor` V.ISOColor 1]

    it "gives up on a stray escape rather than buffering without bound" $ do
      let (stateAfterChunk, segments) = parseAnsi initialAnsiState ("text\ESC[" <> T.replicate 100 "0;")
      segmentsToText segments `shouldBe` "text"
      ansiPendingEscape stateAfterChunk `shouldBe` ""

spec_segmentsHaveColor :: Spec
spec_segmentsHaveColor = do
  describe "segmentsHaveColor" $ do
    it "is False for text that does not color itself" $ do
      segmentsHaveColor (parse "plain text") `shouldBe` False

    it "is True when any segment sets a foreground color" $ do
      segmentsHaveColor (parse "plain \ESC[31mred") `shouldBe` True

    it "is False when only styles are set" $ do
      segmentsHaveColor (parse "\ESC[1mbold") `shouldBe` False
