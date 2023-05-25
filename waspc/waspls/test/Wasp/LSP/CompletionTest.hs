module Wasp.LSP.CompletionTest where

import Control.Lens ((^.))
import Control.Monad.Log.Pure (Log, runLog)
import Control.Monad.State.Strict (StateT, evalStateT)
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
import Wasp.Analyzer.Parser.ConcreteParser (parseCST)
import qualified Wasp.Analyzer.Parser.Lexer as Lexer
import Wasp.LSP.Completion (getCompletionsAtPosition)
import Wasp.LSP.ServerState (ServerState (ServerState, _cst, _currentWaspSource, _latestDiagnostics))

runCompletionTest :: String -> String
runCompletionTest source =
  let (code, position) = readCompletionTest source
      tokens = Lexer.lex code
      parsedCST = snd $ parseCST tokens
      serverState =
        ServerState
          { _currentWaspSource = code,
            _latestDiagnostics = [],
            _cst = Just parsedCST
          }
      (completionItems, _log) = runLog $ evalStateT (getCompletionsAtPosition position) serverState
      fmtedCompletionItems = map fmtCompletionItem completionItems

      fmtCompletionItem :: LSP.CompletionItem -> String
      fmtCompletionItem item =
        concat
          [ "  label={",
            show (item ^. LSP.label),
            "} kind={",
            show (item ^. LSP.kind),
            "} detail={",
            show (item ^. LSP.detail),
            "}"
          ]
   in "Completion Items:\n" ++ unlines fmtedCompletionItems

type CompletionM a = StateT ServerState Log a

-- | Parses a completion test case into a pair of the wasp source code to
-- run completion on and the position to get the completion list at.
--
-- = Format
--
-- A normal wasp file, but with one addition: in one spot in the file, add
-- a "completion marker", which is a "|" with a "^" at the same column on the
-- line below. The line containing "^" should be blank except for whitespace
-- before the "^" and the "^" itself.
--
-- The "|", "^", and the extra line are not part of the wasp source code and
-- are not included in the returned code.
--
-- If there is more than one completion marker in the input, only the first
-- marker is recognized, and the rest are left in the source code.
--
-- === __Example__
--
-- @
-- app todoApp {
--   |
--   ^
-- }
-- @
--
-- This test case checks completions after the 2 spaces on the 2nd line.
readCompletionTest :: String -> (String, LSP.Position)
readCompletionTest source = withPreambleAssert "//! test/completion" source $ (unlines code, position)
  where
    -- Drops the marked line AND the following line (which is blank except for the ^)
    code = take markedLineIdx (lines source) ++ [markedLine] ++ drop (markedLineIdx + 2) (lines source)
    position = LSP.Position (fromIntegral markedLineIdx) (fromIntegral markedColIdx)
    markedLine = take markedColIdx rawMarkedLine ++ drop (markedColIdx + 1) rawMarkedLine
    (rawMarkedLine, markedColIdx, markedLineIdx) =
      case find isMarkedLine candidateLines of
        Nothing -> error "readCompletionTest: no marked line"
        Just x -> x
    candidateLines = mapMaybe (toCandidateLine 0) linePairs
    linePairs = zip3 (lines source) (drop 1 $ lines source) [0 ..]

    isMarkedLine :: (String, Int, Int) -> Bool
    isMarkedLine (str, col, _ln) = (length str >= col) && ((str !! col) == '|')

    toCandidateLine :: Int -> (String, String, Int) -> Maybe (String, Int, Int)
    toCandidateLine n (a, ['^'], ln) = Just (a, n, ln)
    toCandidateLine n (a, ' ' : bs, ln) = toCandidateLine (n + 1) (a, bs, ln)
    toCandidateLine _ _ = Nothing

    withPreambleAssert :: String -> String -> a -> a
    withPreambleAssert preamble str x
      | (preamble ++ "\n") `isPrefixOf` str = x
      | otherwise = error $ "test expected to begin with preamble: " ++ preamble

test_CompletionLists :: IO TestTree
test_CompletionLists = do
  inputFiles <- findByExtension [".wasp"] "./waspls/test/Wasp/LSP/completionTests"
  return $
    testGroup "Wasp.LSP.Completion.getCompletionsAtPosition" $
      makeCompletionTest <$> inputFiles

-- | Create a test case from a .wasp/.golden pair, running runCompletionTest
-- on the .wasp file to get the .golden output. See 'readCompletionTest' for
-- the format of a test case.
makeCompletionTest :: FilePath -> TestTree
makeCompletionTest inputFp =
  let goldenFile = replaceExtension inputFp ".golden"
      testCaseName = takeBaseName inputFp
      diffCmd = \ref new -> ["diff", "-u", ref, new]
   in goldenVsStringDiff
        testCaseName
        diffCmd
        goldenFile
        ( do
            source <- BSC.unpack <$> BS.readFile inputFp
            return $ BSC.pack $ runCompletionTest source
        )
