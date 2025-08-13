module ErrorTest where

import Data.List (intercalate)
import Data.Maybe (fromJust)
import Fixtures (systemSPRoot)
import qualified StrongPath as SP
import Test.Tasty.Hspec
import Wasp.Analyzer.Parser.Ctx (ctxFromRgn)
import Wasp.Analyzer.Parser.SourcePosition (SourcePosition (..))
import Wasp.Error
import qualified Wasp.Util.Terminal as T

spec_WaspError :: Spec
spec_WaspError = do
  describe "showCompilerErrorForTerminal" $ do
    describe "correctly shows simple error" $ do
      let waspFilePath = systemSPRoot SP.</> fromJust (SP.parseRelFile "waspeteer/aproject/main.wasp")
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
        let errCtx = ctxFromRgn (SourcePosition 2 3) (SourcePosition 4 5)
        showCompilerErrorForTerminal (waspFilePath, waspFileContent) (errMsg, errCtx)
          `shouldBe` intercalate
            "\n"
            [ SP.fromAbsFile waspFilePath ++ " @ 2:3 - 4:5",
              "  " ++ errMsg,
              "",
              "  " ++ T.applyStyles [T.Yellow] "     1 | " ++ "app TestApp {",
              "  " ++ T.applyStyles [T.Yellow] "     2 | " ++ "  " ++ T.applyStyles [T.Red] "server:",
              "  " ++ T.applyStyles [T.Yellow] "     3 | " ++ T.applyStyles [T.Red] "    { db: SQLite",
              "  " ++ T.applyStyles [T.Yellow] "     4 | " ++ T.applyStyles [T.Red] "    }" ++ ",",
              "  " ++ T.applyStyles [T.Yellow] "     5 | " ++ "  title: \"Test App\""
            ]
      it "when error spans a single line" $ do
        let errCtx = ctxFromRgn (SourcePosition 5 10) (SourcePosition 5 19)
        showCompilerErrorForTerminal (waspFilePath, waspFileContent) (errMsg, errCtx)
          `shouldBe` intercalate
            "\n"
            [ SP.fromAbsFile waspFilePath ++ " @ 5:10-19",
              "  " ++ errMsg,
              "",
              "  " ++ T.applyStyles [T.Yellow] "     4 | " ++ "    },",
              "  " ++ T.applyStyles [T.Yellow] "     5 | " ++ "  title: " ++ T.applyStyles [T.Red] "\"Test App\"",
              "  " ++ T.applyStyles [T.Yellow] "     6 | " ++ "}"
            ]
      it "when error spans a single character" $ do
        let errCtx = ctxFromRgn (SourcePosition 3 11) (SourcePosition 3 11)
        showCompilerErrorForTerminal (waspFilePath, waspFileContent) (errMsg, errCtx)
          `shouldBe` intercalate
            "\n"
            [ SP.fromAbsFile waspFilePath ++ " @ 3:11",
              "  " ++ errMsg,
              "",
              "  " ++ T.applyStyles [T.Yellow] "     2 | " ++ "  server:",
              "  " ++ T.applyStyles [T.Yellow] "     3 | " ++ "    { db: " ++ T.applyStyles [T.Red] "S" ++ "QLite",
              "  " ++ T.applyStyles [T.Yellow] "     4 | " ++ "    },"
            ]
      it "when there is no context lines around the line with error" $ do
        let waspFileContent' = "app $TestApp { title: \"Test App\" }"
        let errCtx = ctxFromRgn (SourcePosition 1 5) (SourcePosition 1 12)
        showCompilerErrorForTerminal (waspFilePath, waspFileContent') (errMsg, errCtx)
          `shouldBe` intercalate
            "\n"
            [ SP.fromAbsFile waspFilePath ++ " @ 1:5-12",
              "  " ++ errMsg,
              "",
              "  " ++ T.applyStyles [T.Yellow] "     1 | "
                ++ "app "
                ++ T.applyStyles [T.Red] "$TestApp"
                ++ " { title: \"Test App\" }"
            ]
