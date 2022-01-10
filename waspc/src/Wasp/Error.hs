module Wasp.Error (showErrorForTerminal) where

import Data.List (intercalate)
import StrongPath (Abs, File', Path')
import qualified StrongPath as SP
import Wasp.Analyzer.Parser.Ctx (Ctx, getCtxRgn)
import Wasp.Analyzer.Parser.SourcePosition (SourcePosition (..))
import Wasp.Analyzer.Parser.SourceRegion (SourceRegion (..))
import Wasp.Util (indent, insertAt, leftPad)
import qualified Wasp.Util.Terminal as T

showErrorForTerminal :: (Path' Abs File', String) -> (String, Ctx) -> String
showErrorForTerminal (waspFilePath, waspFileContent) (errMsg, errCtx) =
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
prettyShowSrcLinesOfErrorRgn :: String -> SourceRegion -> String
prettyShowSrcLinesOfErrorRgn
  waspFileContent
  ( SourceRegion
      (SourcePosition startLineIdx startColIdx)
      (SourcePosition endLineIdx endColIdx)
    ) =
    let srcLines =
          zip [startLineIdx ..] $
            take (endLineIdx - startLineIdx + 1) $
              drop startLineIdx (lines waspFileContent)
        srcLinesWithMarkedErrorRgn =
          map
            ( \(lineIdx, line) ->
                let stylingEnd = T.escapeCode ++ T.resetCode
                    stylingStart = T.escapeCode ++ T.styleCode T.Red
                    lineWithStylingEnd =
                      if lineIdx == endLineIdx
                        then insertAt stylingEnd (endColIdx + 1) line
                        else line ++ stylingEnd
                    lineWithStylingStartAndEnd =
                      if lineIdx == startLineIdx
                        then insertAt stylingStart startColIdx lineWithStylingEnd
                        else stylingStart ++ lineWithStylingEnd
                 in (lineIdx, lineWithStylingStartAndEnd)
            )
            srcLines
        srcLinesWithMarkedErrorRgnAndLineNumber =
          map
            (\(lineIdx, line) -> T.applyStyles [T.Yellow] (leftPad ' ' 6 (show lineIdx) ++ " | ") ++ line)
            srcLinesWithMarkedErrorRgn
     in intercalate "\n" srcLinesWithMarkedErrorRgnAndLineNumber
