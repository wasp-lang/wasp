module Wasp.Error (showError) where

import Data.List (intercalate)
import StrongPath (Abs, File', Path')
import qualified StrongPath as SP
import Wasp.Analyzer.Parser.Ctx (Ctx, getCtxRgn)
import Wasp.Analyzer.Parser.SourcePosition (SourcePosition (..))
import Wasp.Analyzer.Parser.SourceRegion (SourceRegion (..))
import Wasp.Util (indent)

-- TODO: Consider polishing this a bit.
showError :: (Path' Abs File', String) -> (String, Ctx) -> String
showError (waspFilePath, waspFileContent) (errMsg, errCtx) =
  let srcRegion = getCtxRgn errCtx
   in intercalate
        "\n"
        [ SP.fromAbsFile waspFilePath ++ " @ " ++ showRgn srcRegion,
          indent 2 errMsg,
          "",
          indent 2 $ intercalate "\n" $ getEnumedSrcLinesOfRgn srcRegion
        ]
  where
    showPos :: SourcePosition -> String
    showPos (SourcePosition l c) = show l ++ ":" ++ show c

    showRgn :: SourceRegion -> String
    showRgn (SourceRegion startPos endPos) =
      if startPos == endPos
        then showPos startPos
        else showPos startPos ++ " - " ++ showPos endPos

    getEnumedSrcLinesOfRgn :: SourceRegion -> [String]
    getEnumedSrcLinesOfRgn
      ( SourceRegion
          (SourcePosition startLineIdx startColIdx)
          (SourcePosition endLineIdx endColIdx)
        ) =
        let srcLines =
              take (endLineIdx - startLineIdx + 1) $
                drop startLineIdx (lines waspFileContent)
            enumedSrcLines =
              zipWith
                (\lineIdx line -> pad 6 (show lineIdx) ++ " | " ++ line)
                [startLineIdx ..]
                srcLines
            -- TODO: make this 9 not hardcoded but calculated from 6 and " | " above.
            multiLineRgnStartIndicator = replicate 9 ' ' ++ replicate startColIdx ' ' ++ "v"
            multiLineRgnEndIndicator = replicate 9 ' ' ++ replicate endColIdx ' ' ++ "^"
            singleLineRgnStartAndEndIndicator =
              replicate 9 ' '
                ++ replicate startColIdx ' '
                ++ replicate (endColIdx - startColIdx + 1) 'v'
         in if length enumedSrcLines == 1
              then singleLineRgnStartAndEndIndicator : enumedSrcLines
              else
                multiLineRgnStartIndicator :
                enumedSrcLines
                  ++ [multiLineRgnEndIndicator]

    pad :: Int -> String -> String
    pad n str =
      let padded = replicate n ' ' ++ str
       in drop (length padded - n) padded
