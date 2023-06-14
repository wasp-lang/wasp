module Wasp.LSP.CompletionTest where

import Control.Lens ((^.))
import Control.Monad (guard)
import Control.Monad.Log (runLog)
import Control.Monad.Reader (runReaderT)
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Lazy.Char8 as BSC
import Data.List (elemIndex, isPrefixOf)
import Data.Maybe (listToMaybe, mapMaybe)
import qualified Language.LSP.Types as LSP
import qualified Language.LSP.Types.Lens as LSP
import System.FilePath (replaceExtension, takeBaseName)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Golden (findByExtension, goldenVsStringDiff)
import Text.Printf (printf)
import Wasp.Analyzer.Parser.ConcreteParser (parseCST)
import qualified Wasp.Analyzer.Parser.Lexer as Lexer
import Wasp.LSP.Completion (getCompletionsAtPosition)
import Wasp.LSP.ServerState (ServerState (ServerState, _cst, _currentWaspSource, _latestDiagnostics, _reactorIn, _regTokens, _tsExports))

-- | A string containing the input to a completion test. It represents wasp
-- source code with a cursor position.
--
-- = Format
--
-- A normal wasp file, but with two additions:
-- 1) Begins with "//! test/completion" (this is future-proofing in case we
--    add editor support for these test files).
-- 2) In one spot in the file, add a "completion marker", which is a "|" with
--    a "^" at the same column on the line below. The line containing "^" should
--    be blank except for whitespace before the "^" and the "^" itself. The line
--    containing "^" is referred to as the "carrot line".
--
-- The "|", "^", and the extra line are not part of the wasp source code and
-- are not included in the returned code. The preamble comment is included in
-- the returned code.
--
-- If there is more than one completion marker in the input, only the first
-- marker is recognized, and the rest are left in the source code.
--
-- === __Example__
--
-- @
-- //! test/completion
-- app todoApp {
--   |
--   ^
-- }
-- @
--
-- This test checks completions after the 2 spaces on the 2nd line.
newtype CompletionTestInput = CompletionTestInput String

-- | Run test cases in ./completionTests directory.
--
-- See 'CompletionTestInput' for the format of the test input (stored in the
-- .wasp files).
test_CompletionLists :: IO TestTree
test_CompletionLists = do
  -- Path is relative to the root of the project, i.e. the directory containing
  -- waspc.cabal.
  completionTestInputFiles <- findByExtension [".wasp"] "./waspls/test/Wasp/LSP/completionTests"
  return $
    testGroup "Wasp.LSP.Completion.getCompletionsAtPosition" $
      makeGoldenCompletionTest <$> completionTestInputFiles

-- | Create a test case from a pair of <test_name>.wasp and <test_name>.golden
-- file, using 'runCompletionTest' on the .wasp file to get the output to
-- compare with or store in the .golden file.
--
-- The given filepath should be to a completion test input file (ending in .wasp)
-- inside the completionTests folder.
makeGoldenCompletionTest :: FilePath -> TestTree
makeGoldenCompletionTest testInputFile =
  let goldenFile = replaceExtension testInputFile ".golden"
      testCaseName = takeBaseName testInputFile
      diffCmd = \ref new -> ["diff", "-u", ref, new]
      testOutput = BSC.pack . runCompletionTest <$> readTestInputFile testInputFile
   in goldenVsStringDiff
        testCaseName
        diffCmd
        goldenFile
        testOutput

-- | @readTestInputFile testInputFile@ takes a path to a completion test input
-- file (a .wasp file in the completionTests directory).
readTestInputFile :: FilePath -> IO CompletionTestInput
readTestInputFile testInputFile =
  CompletionTestInput . BSC.unpack <$> BS.readFile testInputFile

-- | Takes a completion test input and produces the list of completion items,
-- represented as a string so it can be compared with the contents of and stored
-- in the golden file associated with the test input.
runCompletionTest :: CompletionTestInput -> String
runCompletionTest testInput =
  let (waspSource, cursorPosition) = parseCompletionInput testInput
      tokens = Lexer.lex waspSource
      parsedCST = snd $ parseCST tokens
      serverState =
        ServerState
          { _currentWaspSource = waspSource,
            _latestDiagnostics = [],
            _cst = Just parsedCST,
            _tsExports = error "_tsExports not available in completion tests",
            _reactorIn = error "_reactorIn not available in completion tests",
            _regTokens = error "_regTokens not available in completion tests"
          }
      (completionItems, _log) = runLog $ runReaderT (getCompletionsAtPosition cursorPosition) serverState
      fmtedCompletionItems = map fmtCompletionItem completionItems

      fmtCompletionItem :: LSP.CompletionItem -> String
      fmtCompletionItem item =
        unwords
          [ printf "label={%s}" (show $ item ^. LSP.label),
            printf "kind={%s}" (show $ item ^. LSP.kind),
            printf "detail={%s}" (show $ item ^. LSP.detail)
          ]
   in "Completion items:\n" ++ unlines (map ("  " <>) fmtedCompletionItems)

-- | Parses a completion test case into a pair of the wasp source code to
-- run completion on and the position to get the completion list at.
--
-- See 'CompletionTestInput' for the format of the test input.
parseCompletionInput :: CompletionTestInput -> (String, LSP.Position)
parseCompletionInput (CompletionTestInput waspSourceWithCursor) =
  if not (hasCompletionTestPreamble waspSourceWithCursor)
    then error missingCompletionTestPreambleMsg
    else (waspSourceCode, cursorPosition)
  where
    waspSourceCode =
      unlines . concat $
        [ linesBeforeCursor,
          [cursorLineWithCursorRemoved],
          linesAfterCursor
        ]
    cursorPosition = LSP.Position (fromIntegral cursorLineIdx) (fromIntegral cursorColumnIdx)

    linesBeforeCursor = take cursorLineIdx (lines waspSourceWithCursor)
    -- NOTE: drop cursorLineIdx + 2 so that we skip the "   ^" line after the cursor.
    linesAfterCursor = drop (cursorLineIdx + 2) (lines waspSourceWithCursor)

    cursorLineWithCursorRemoved =
      let cursorLine = lines waspSourceWithCursor !! cursorLineIdx
       in take cursorColumnIdx cursorLine <> drop (cursorColumnIdx + 1) cursorLine

    (cursorLineIdx, cursorColumnIdx) =
      case listToMaybe $ mapMaybe cursorColumnIdxFromLinePair enumeratedLinePairs of
        Nothing -> error "parseCompletionInput: found no cursor!"
        Just v -> v
    enumeratedLinePairs =
      zip [0 ..] $ zip (lines waspSourceWithCursor) (drop 1 . lines $ waspSourceWithCursor)

    -- Given two consecutive lines from the test input, along with the line number
    -- of the first line, look for a cursor in the line pair and find the index
    -- of the column the cursor is at.
    cursorColumnIdxFromLinePair :: (Int, (String, String)) -> Maybe (Int, Int)
    cursorColumnIdxFromLinePair (line1Idx, (line1, line2)) = do
      cursorColIdx <- elemIndex '|' line1
      guard $ line2 == replicate cursorColIdx ' ' <> "^"
      return (line1Idx, cursorColIdx)

    hasCompletionTestPreamble :: String -> Bool
    hasCompletionTestPreamble source = (completionTestPreamble <> "\n") `isPrefixOf` source

    missingCompletionTestPreambleMsg = "parseCompletionInput: missing preamble: " <> completionTestPreamble

    completionTestPreamble = "//! test/completion"
