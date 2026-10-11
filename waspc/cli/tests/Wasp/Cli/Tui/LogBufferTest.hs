module Wasp.Cli.Tui.LogBufferTest where

import Data.Foldable (toList)
import qualified Data.Text as T
import Test.Hspec
import Wasp.Cli.Tui.LogBuffer
  ( LogBuffer,
    LogLevel (..),
    LogLine (..),
    appendChunk,
    bufferedLines,
    clearLogBuffer,
    emptyLogBuffer,
    numBufferedLines,
    numDroppedLines,
  )

-- | Feeds chunks as stdout and returns the text of the lines that ended up in
-- the buffer.
feedStdout :: [T.Text] -> [T.Text]
feedStdout = map logLineText . toList . bufferedLines . foldl append emptyLogBuffer
  where
    append buffer chunk = fst (appendChunk False chunk buffer)

spec_appendChunk :: Spec
spec_appendChunk = do
  describe "appendChunk" $ do
    it "commits a line once its newline arrives" $ do
      feedStdout ["one\ntwo\n"] `shouldBe` ["one", "two"]

    it "holds back a line that has no newline yet" $ do
      feedStdout ["partial"] `shouldBe` []

    it "joins a line that arrives in several chunks" $ do
      feedStdout ["par", "tial ", "line\n"] `shouldBe` ["partial line"]

    it "normalizes CRLF line endings" $ do
      feedStdout ["one\r\ntwo\r\n"] `shouldBe` ["one", "two"]

    it "lets a carriage return overwrite the line so far" $ do
      -- How progress bars redraw themselves in place.
      feedStdout ["10%\r50%\r100%\n"] `shouldBe` ["100%"]

    it "keeps the two streams' partial lines apart" $ do
      let buffer = fst $ appendChunk False "out" emptyLogBuffer
          buffer' = fst $ appendChunk True "err\n" buffer
          buffer'' = fst $ appendChunk False "put\n" buffer'
      map logLineText (toList (bufferedLines buffer'')) `shouldBe` ["err", "output"]

    it "marks which stream a line came from" $ do
      let buffer = fst $ appendChunk True "oops\n" emptyLogBuffer
      map logLineIsStderr (toList (bufferedLines buffer)) `shouldBe` [True]

    it "returns the lines a chunk completed" $ do
      let (_, newLines) = appendChunk False "one\ntwo\nthre" emptyLogBuffer
      map logLineText newLines `shouldBe` ["one", "two"]

    it "drops the oldest lines once the buffer is full" $ do
      let chunk = T.concat [T.pack (show n) <> "\n" | n <- [1 :: Int .. 6000]]
          buffer = fst $ appendChunk False chunk emptyLogBuffer
      numBufferedLines buffer `shouldBe` 5000
      numDroppedLines buffer `shouldBe` 1000
      map logLineText (take 1 (toList (bufferedLines buffer))) `shouldBe` ["1001"]

spec_clearLogBuffer :: Spec
spec_clearLogBuffer = do
  describe "clearLogBuffer" $ do
    it "empties the buffer but still delivers a line that was in flight" $ do
      let buffer = fst $ appendChunk False "done\nhalf" emptyLogBuffer
          cleared = clearLogBuffer buffer :: LogBuffer
          finished = fst $ appendChunk False " a line\n" cleared
      map logLineText (toList (bufferedLines finished)) `shouldBe` ["half a line"]

spec_detectLogLevel :: Spec
spec_detectLogLevel = do
  describe "log level detection" $ do
    let levelOf text = map logLineLevel (toList (bufferedLines (fst (appendChunk False (text <> "\n") emptyLogBuffer))))

    it "flags errors" $ do
      levelOf "Error: something broke" `shouldBe` [LevelError]
      levelOf "npm ERR! install failed" `shouldBe` [LevelError]

    it "flags warnings" $ do
      levelOf "warning: deprecated api" `shouldBe` [LevelWarn]

    it "flags success" $ do
      levelOf "Server listening on port 3001" `shouldBe` [LevelSuccess]
      levelOf "VITE ready in 133 ms" `shouldBe` [LevelSuccess]

    it "leaves ordinary lines alone" $ do
      levelOf "> npm run build" `shouldBe` [LevelNormal]

    it "does not flag an announcement that there were no errors" $ do
      levelOf "Found 0 errors." `shouldBe` [LevelNormal]

    it "notices when a line colors itself" $ do
      let selfColored = map logLineIsSelfColored . toList . bufferedLines . fst
      selfColored (appendChunk False "\ESC[31mred\ESC[0m\n" emptyLogBuffer) `shouldBe` [True]
      selfColored (appendChunk False "plain\n" emptyLogBuffer) `shouldBe` [False]
