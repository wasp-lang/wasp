module Wasp.LSP.CompletionTest where

import Control.Lens ((^.))
import Control.Monad.Log (runLog)
import Control.Monad.State.Strict (evalStateT)
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Lazy.Char8 as BSC
import Data.Foldable (find)
import Data.List (isPrefixOf)
import Data.Maybe (mapMaybe)
import qualified Language.LSP.Types as LSP
import qualified Language.LSP.Types.Lens as LSP
import System.FilePath (replaceExtension, takeBaseName)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Golden (findByExtension, goldenVsStringDiff)
import Text.Printf (printf)
import Wasp.Analyzer.Parser.ConcreteParser (parseCST)
import qualified Wasp.Analyzer.Parser.Lexer as Lexer
import Wasp.LSP.Completion (getCompletionsAtPosition)
import Wasp.LSP.ServerState (ServerState (ServerState, _cst, _currentWaspSource, _latestDiagnostics))

-- | A string containing the input to a completion test. It represents wasp
-- source code with a cursor position.
--
-- = Format
--
-- A normal wasp file, but with two addition
-- 1) Begins with "//! test/completion" (this is future-proofing in case we
--    add editor support for these test files).
-- 2) In one spot in the file, add a "completion marker", which is a "|" with
-- a "^" at the same column on the line below. The line containing "^" should be
-- blank except for whitespace before the "^" and the "^" itself.
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

-- | Run test cases in ./completionTests directory
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
makeGoldenCompletionTest inputFp =
  let goldenFile = replaceExtension inputFp ".golden"
      testCaseName = takeBaseName inputFp
      diffCmd = \ref new -> ["diff", "-u", ref, new]
   in goldenVsStringDiff
        testCaseName
        diffCmd
        goldenFile
        ( do
            source <- BSC.unpack <$> BS.readFile inputFp
            return $ BSC.pack $ runCompletionTest $ CompletionTestInput source
        )

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
            _cst = Just parsedCST
          }
      (completionItems, _log) = runLog $ evalStateT (getCompletionsAtPosition cursorPosition) serverState
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
parseCompletionInput (CompletionTestInput testInput) =
  withPreambleAssert "//! test/completion" testInput (unlines code, position)
  where
    -- Drops the marked line AND the following line (which is blank except for the ^)
    code = before markedLineIdx (lines testInput) ++ [markedLine] ++ after (markedLineIdx + 1) (lines testInput)
    position = LSP.Position (fromIntegral markedLineIdx) (fromIntegral markedColIdx)

    markedLine = before markedColIdx rawMarkedLine ++ after markedColIdx rawMarkedLine
    (rawMarkedLine, markedColIdx, markedLineIdx) =
      case find isMarkedLine candidateLines of
        Nothing -> error "readCompletionTest: no marked line"
        Just x -> x

    -- (String, column, line) triples, where String is a line such that the
    -- following line is spaces followed by a ^ and nothing else
    candidateLines = mapMaybe (toCandidateLine 0) linePairs
    -- Pairs of consecutive lines in the testInput
    linePairs = zip3 (lines testInput) (drop 1 $ lines testInput) [0 ..]

    -- Check if the candidate line contains a '|' at the specified column
    isMarkedLine :: (String, Int, Int) -> Bool
    isMarkedLine (str, col, _ln) = (length str >= col) && ((str !! col) == '|')

    -- Convert a pair of lines into a candidate line (this checks if the second
    -- line is spaces followed by a ^ and nothing else)
    toCandidateLine :: Int -> (String, String, Int) -> Maybe (String, Int, Int)
    toCandidateLine n (a, ['^'], ln) = Just (a, n, ln)
    toCandidateLine n (a, ' ' : bs, ln) = toCandidateLine (n + 1) (a, bs, ln)
    toCandidateLine _ _ = Nothing

    withPreambleAssert :: String -> String -> a -> a
    withPreambleAssert preamble str x
      | (preamble ++ "\n") `isPrefixOf` str = x
      | otherwise = error $ "test expected to begin with preamble: " ++ preamble

    before :: Int -> [a] -> [a]
    before = take

    after :: Int -> [a] -> [a]
    after idx = drop (idx + 1)
