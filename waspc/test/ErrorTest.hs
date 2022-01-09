module ErrorTest where

import Data.List (intercalate)
import Data.Maybe (fromJust)
import qualified StrongPath as SP
import Test.Tasty.Hspec
import qualified Wasp.Analyzer.AnalyzeError as AE
import Wasp.Analyzer.Parser.Ctx (Ctx, ctxFromRgn)
import qualified Wasp.Analyzer.Parser.ParseError as PE
import Wasp.Analyzer.Parser.SourcePosition (SourcePosition (..))
import Wasp.Analyzer.Parser.SourceRegion (SourceRegion (..))
import Wasp.Error

spec_WaspError :: Spec
spec_WaspError = do
  describe "showError" $ do
    describe "correctly shows simple error" $ do
      let waspFilePath = fromJust $ SP.parseAbsFile "/home/waspeteer/aproject/main.wasp"
      let waspFileContent =
            unlines
              [ "app TestApp {",
                "  server:",
                "    { db: SQLite",
                "    },",
                "  title: \"Test App\"",
                "}"
              ]
      let errMsg = "Whoops: a test error happened!"
      it "when error spans multiple lines" $ do
        let errCtx = ctxFromRgn (SourcePosition 1 2) (SourcePosition 3 4)
        showError (waspFilePath, waspFileContent) (errMsg, errCtx)
          `shouldBe` intercalate
            "\n"
            [ "/home/waspeteer/aproject/main.wasp @ 1:2 - 3:4",
              "  Whoops: a test error happened!",
              "",
              "             v", -- TODO: Make it more like haskell errors, where 'v' goes all the way to the end, and '^' start from the very beginning of line?
              "       1 |   server:",
              "       2 |     { db: SQLite",
              "       3 |     },",
              "               ^"
            ]
      it "when error spans a single line" $ do
        let errCtx = ctxFromRgn (SourcePosition 4 9) (SourcePosition 4 18)
        showError (waspFilePath, waspFileContent) (errMsg, errCtx)
          `shouldBe` intercalate
            "\n"
            [ "/home/waspeteer/aproject/main.wasp @ 4:9 - 4:18", -- TODO: Show region as 4:9-18?
              "  Whoops: a test error happened!",
              "",
              "                    vvvvvvvvvv",
              "       4 |   title: \"Test App\""
            ]
      it "when error spans a single character" $ do
        let errCtx = ctxFromRgn (SourcePosition 2 10) (SourcePosition 2 10)
        showError (waspFilePath, waspFileContent) (errMsg, errCtx)
          `shouldBe` intercalate
            "\n"
            [ "/home/waspeteer/aproject/main.wasp @ 2:10",
              "  Whoops: a test error happened!",
              "",
              "                     v",
              "       2 |     { db: SQLite"
            ]
