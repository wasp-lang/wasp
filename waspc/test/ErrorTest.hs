module ErrorTest where

import Data.List (intercalate)
import Data.Maybe (fromJust)
import qualified StrongPath as SP
import Test.Tasty.Hspec
import Wasp.Analyzer.Parser.Ctx (ctxFromRgn)
import Wasp.Analyzer.Parser.SourcePosition (SourcePosition (..))
import Wasp.Error
import qualified Wasp.Util.Terminal as T

spec_WaspError :: Spec
spec_WaspError = do
  describe "showErrorForTerminal" $ do
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
        showErrorForTerminal (waspFilePath, waspFileContent) (errMsg, errCtx)
          `shouldBe` intercalate
            "\n"
            [ "/home/waspeteer/aproject/main.wasp @ 1:2 - 3:4",
              "  Whoops: a test error happened!",
              "",
              "  " ++ T.applyStyles [T.Yellow] "     1 | " ++ "  " ++ T.applyStyles [T.Red] "server:",
              "  " ++ T.applyStyles [T.Yellow] "     2 | " ++ T.applyStyles [T.Red] "    { db: SQLite",
              "  " ++ T.applyStyles [T.Yellow] "     3 | " ++ T.applyStyles [T.Red] "    }" ++ ","
            ]
      it "when error spans a single line" $ do
        let errCtx = ctxFromRgn (SourcePosition 4 9) (SourcePosition 4 18)
        showErrorForTerminal (waspFilePath, waspFileContent) (errMsg, errCtx)
          `shouldBe` intercalate
            "\n"
            [ "/home/waspeteer/aproject/main.wasp @ 4:9-18",
              "  Whoops: a test error happened!",
              "",
              "  " ++ T.applyStyles [T.Yellow] "     4 | " ++ "  title: " ++ T.applyStyles [T.Red] "\"Test App\""
            ]
      it "when error spans a single character" $ do
        let errCtx = ctxFromRgn (SourcePosition 2 10) (SourcePosition 2 10)
        showErrorForTerminal (waspFilePath, waspFileContent) (errMsg, errCtx)
          `shouldBe` intercalate
            "\n"
            [ "/home/waspeteer/aproject/main.wasp @ 2:10",
              "  Whoops: a test error happened!",
              "",
              "  " ++ T.applyStyles [T.Yellow] "     2 | " ++ "    { db: " ++ T.applyStyles [T.Red] "S" ++ "QLite"
            ]
