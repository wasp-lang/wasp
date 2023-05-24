module Wasp.LSP.CompletionTest where

import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Lazy.Char8 as BSC
import Data.Foldable (find)
import Data.Maybe (mapMaybe)
import qualified Language.LSP.Types as LSP
import qualified Language.LSP.Types.Lens as LSP
import System.FilePath (replaceExtension, takeBaseName)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Golden (findByExtension, goldenVsStringDiff)

runCompletionTest :: String -> String
runCompletionTest source = show $ readCompletionTest source

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
readCompletionTest source = (unlines code, position)
  where
    code = take (markedLineIdx - 1) (lines source) ++ [markedLine] ++ drop markedLineIdx (lines source)
    position = LSP.Position (fromIntegral markedLineIdx) (fromIntegral markedColIdx)
    markedLine = take (markedColIdx - 1) rawMarkedLine ++ drop markedColIdx rawMarkedLine
    (rawMarkedLine, markedColIdx, markedLineIdx) =
      case find isMarkedLine candidateLines of
        Nothing -> error "readCompletionTest: no marked line"
        Just x -> x
    candidateLines = mapMaybe (toCandidateLine 1) linePairs
    linePairs = zip3 (lines source) (drop 1 $ lines source) [1 ..]

    isMarkedLine :: (String, Int, Int) -> Bool
    isMarkedLine (str, col, _ln) = (length str >= (col - 1)) && ((str !! (col - 1)) == '|')

    toCandidateLine :: Int -> (String, String, Int) -> Maybe (String, Int, Int)
    toCandidateLine n (a, ['^'], ln) = Just (a, n, ln)
    toCandidateLine n (a, ' ' : bs, ln) = toCandidateLine (n + 1) (a, bs, ln)
    toCandidateLine _ _ = Nothing

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
