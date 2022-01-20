module Wasp.Error (showCompilerErrorForTerminal) where

import Data.List (intercalate)
import StrongPath (Abs, File', Path')
import qualified StrongPath as SP
import Wasp.Analyzer.Parser.Ctx (Ctx, getCtxRgn)
import Wasp.Analyzer.Parser.SourcePosition (SourcePosition (..))
import Wasp.Analyzer.Parser.SourceRegion (SourceRegion (..))
import Wasp.Util (indent, insertAt, leftPad)
import qualified Wasp.Util.Terminal as T

-- | Transforms compiler error (error with parse context) into an informative, pretty String that
-- can be printed directly into the terminal. It uses terminal features like escape codes
-- (colors, styling, ...).
showCompilerErrorForTerminal :: (Path' Abs File', String) -> (String, Ctx) -> String
showCompilerErrorForTerminal (waspFilePath, waspFileContent) (errMsg, errCtx) =
  let srcRegion = getCtxRgn errCtx
   in intercalate
        "\n"
        [ SP.fromAbsFile waspFilePath ++ " @ " ++ showRgn srcRegion,
          indent 2 errMsg,
          "",
          indent 2 $ prettyShowSrcLinesOfErrorRgn waspFileContent srcRegion
        ]

showRgn :: SourceRegion -> String
showRgn (SourceRegion (SourcePosition l1 c1) (SourcePosition l2 c2))
  | l1 == l2 && c1 == c2 = showPos l1 c1
  | l1 == l2 && c1 /= c2 = show l1 ++ ":" ++ show c1 ++ "-" ++ show c2
  | otherwise = showPos l1 c1 ++ " - " ++ showPos l2 c2
  where
    showPos l c = show l ++ ":" ++ show c

-- | Given wasp source and error region in it, extracts source lines
-- that are in the given error region and then nicely displays them,
-- by coloring in red the exact error region part of the code and also
-- by prefixing all the lines with their line number (colored yellow).
-- Uses terminal features for styling, like escape codes and similar.
prettyShowSrcLinesOfErrorRgn :: String -> SourceRegion -> String
prettyShowSrcLinesOfErrorRgn
  waspFileContent
  ( SourceRegion
      (SourcePosition startLineNum startColNum)
      (SourcePosition endLineNum endColNum)
    ) =
    let srcLines =
          zip [max 1 (startLineNum - numCtxLines) ..] $
            take (endLineNum - startLineNum + 1 + numCtxLines * 2) $
              drop (startLineNum - 1 - numCtxLines) $
                lines waspFileContent
        srcLinesWithMarkedErrorRgn =
          map
            ( \(lineNum, line) ->
                let lineContainsError = lineNum >= startLineNum && lineNum <= endLineNum
                    lineWithStylingStartAndEnd =
                      if lineNum == startLineNum
                        then insertAt stylingStart (startColNum - 1) lineWithStylingEnd
                        else stylingStart ++ lineWithStylingEnd
                    lineWithStylingEnd =
                      if lineNum == endLineNum
                        then insertAt stylingEnd endColNum line
                        else line ++ stylingEnd
                    stylingStart = T.escapeCode ++ T.styleCode T.Red
                    stylingEnd = T.escapeCode ++ T.resetCode
                 in (lineNum, if lineContainsError then lineWithStylingStartAndEnd else line)
            )
            srcLines
        srcLinesWithMarkedErrorRgnAndLineNumber =
          map
            (\(lineNum, line) -> T.applyStyles [T.Yellow] (leftPad ' ' 6 (show lineNum) ++ " | ") ++ line)
            srcLinesWithMarkedErrorRgn
     in intercalate "\n" srcLinesWithMarkedErrorRgnAndLineNumber
    where
      -- Number of lines to show before and after the source region containing error.
      numCtxLines :: Int
      numCtxLines = 1
