{-# LANGUAGE NamedFieldPuns #-}

module Main where

import Control.Monad (forM, forM_)
import Data.Functor ((<&>))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T.IO
import Numeric.Natural (Natural)
import Text.Printf (printf)

-- TODO: Use StrongPath

main :: IO ()
main = do
  let srcFilePaths = [] -- TODO: Somehow provide some files here!
  codeRefsWithCtxt <- concat <$> forM srcFilePaths readCodeRefs
  forM_
    codeRefsWithCtxt
    ( \CodeRefWithCtxt {docFilePath, commentLineIdx, codeRef} -> do
        message <-
          checkCodeRef codeRef <&> \case
            True -> "good" :: String
            False -> "MISMATCH" :: String
        putStrLn $ printf "%s - L%-4d: %s" docFilePath commentLineIdx message
    )

data CodeRefWithCtxt = CodeRefWithCtxt
  { docFilePath :: FilePath,
    commentLineIdx :: LineIdx,
    codeRef :: CodeRef
  }

data CodeRef = CodeRef
  { srcCode :: Code,
    refFilePath :: FilePath,
    refCodeRegion :: CodeRegion
  }

type Code = Text

type LineIdx = Natural

data CodeRegion = LineRange LineIdx LineIdx

-- Probably all the files that match provided extensions (.md, .mdx, .tsx).
listDocFiles :: IO [FilePath]
listDocFiles = undefined
  where
    -- TODO: Make this path not hardcoded. Paths module?
    docsDir :: FilePath
    docsDir = "/home/martin/git/wasp-lang/waspc/web/docs/"

readCodeRefs :: FilePath -> IO [CodeRefWithCtxt]
readCodeRefs = undefined

-- TODO: What we could do is parse all coderef comments and also parse all code blocks.
--   Then we match those together into pairs. If there are any coderef comments or blocks
--   that don't have a pair, we return an error message for those (will have to change return type for that).
--   Hm we might in that case want to allow for special coderef comment that says no coderef is needed
--   for the code block below it. For such block we would not report warning then, we would just skip it.
parseCodeRefs :: Text -> [(LineIdx, CodeRef)]
parseCodeRefs = undefined

parseCodeRefComment :: Text -> Maybe (FilePath, CodeRegion)
parseCodeRefComment = undefined
  where
    specialCommentRegex :: String
    specialCommentRegex = "^\\s*//\\s*CODEREF\\s+"

-- TODO: Be smarter about checking
--   - Trim start/end.
--   - If there is difference in indentation for the first line, shift one code block to match another.
--   - Allow for executing an awk(or sed?) command that modifies the src code.
-- TODO: Optimize reading of ref code blocks by reading all the blocks in one batch (so each file is
-- read only once).
checkCodeRef :: CodeRef -> IO Bool
checkCodeRef codeRef = do
  let srcCode = codeRef.srcCode
  refCode <- readCodeRegion codeRef.refFilePath codeRef.refCodeRegion
  pure $ srcCode == refCode

readCodeRegion :: FilePath -> CodeRegion -> IO Code
readCodeRegion filePath (LineRange firstLineIdx lastLineIdx) = do
  T.unlines
    . take (fromIntegral (lastLineIdx - firstLineIdx + 1))
    . drop (fromIntegral firstLineIdx)
    . T.lines
    <$> T.IO.readFile filePath
