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
    it "correctly shows simple error" $ do
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
      let errCtx = ctxFromRgn (SourcePosition 1 2) (SourcePosition 3 4)
      showError (waspFilePath, waspFileContent) (errMsg, errCtx)
        `shouldBe` intercalate
          "\n"
          [ "/home/waspeteer/aproject/main.wasp @ 1:2 - 3:4",
            "  Whoops: a test error happened!",
            "",
            "             v",
            "       1 |   server:",
            "       2 |     { db: SQLite",
            "       3 |     },",
            "               ^"
          ]

-- TODO: Write a test for case when error is only in a single line.
