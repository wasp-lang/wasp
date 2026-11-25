module TranscriptTest.Normalization
  ( RuntimeOutput (..),
    NormalizationOptions (..),
    normalizeOutput,
    containsRuntimeOutputOfWholeApp,
  )
where

import Control.Monad (guard)
import Data.Bifunctor (first)
import Data.Char (isAlpha)
import Data.Function (on)
import Data.List (dropWhileEnd, find, groupBy)
import Data.Maybe (isJust, mapMaybe)
import qualified Data.Text as T

-- | What to do with the output of the user's running app, which is different
-- on every run.
data RuntimeOutput
  = -- | The command doesn't run the app.
    NoRuntimeOutput
  | -- | Keeps only what was printed before the app started printing.
    CutOffAtRuntimeOutput
  | -- | Keeps what was printed before the app started printing, and from then
    -- on only what Wasp itself printed.
    DropRuntimeOutput

data NormalizationOptions = NormalizationOptions
  { runtimeOutput :: RuntimeOutput,
    -- | Shows up in the output as an absolute path, which is different on
    -- every machine.
    scenarioDir :: FilePath
  }

-- | Keeps what Wasp itself printed as it is, and replaces everything that
-- changes between runs or machines with placeholders.
normalizeOutput :: NormalizationOptions -> T.Text -> T.Text
normalizeOutput options =
  makeControlCharactersVisible
    . normalizeLines options.runtimeOutput
    . T.replace (T.pack options.scenarioDir) "<scenario-dir>"
    . dropErasedLines
    . dropCtrlDEcho
    . dropTerminalLineEndings
  where
    -- A terminal turns every "\n" a program prints into "\r\n".
    dropTerminalLineEndings = T.replace "\r\n" "\n"
    -- Some terminals (e.g. on macOS) show Ctrl-D as "^D" and then move the
    -- cursor back over it. Others show nothing.
    dropCtrlDEcho = T.replace "^D\b\b" ""

-- | Wasp redraws its live line (e.g. a spinner) many times, a different number
-- of times on every run. Each redraw starts by erasing the line, so only what
-- was written after the last erasing is what stays on the screen.
dropErasedLines :: T.Text -> T.Text
dropErasedLines = T.intercalate "\n" . map (snd . T.breakOnEnd eraseLineSequence) . T.splitOn "\n"
  where
    eraseLineSequence = "\r\ESC[2K"

normalizeLines :: RuntimeOutput -> T.Text -> T.Text
normalizeLines runtimeOutput output = case runtimeOutput of
  -- Whether the output ends with a newline is a part of what Wasp printed.
  NoRuntimeOutput -> mapLinesKeepingLastLineEnding collapseJobOutput output
  CutOffAtRuntimeOutput -> T.unlines $ collapseJobOutput linesBeforeRuntimeOutput ++ [cutOffMarker]
  DropRuntimeOutput -> T.unlines $ collapseJobOutput linesBeforeRuntimeOutput ++ keepOnlyWaspOutput linesFromRuntimeOutput
  where
    outputLines = case runtimeOutput of
      DropRuntimeOutput -> T.lines $ dropUnfinishedAnsiCodeAtEnd output
      _keepsTheEndAsItIs -> T.lines output
    -- A job's output can start with an empty line, which is printed without
    -- the job's prefix.
    linesBeforeRuntimeOutput = dropWhileEnd isEmptyLine linesBeforeFirstRuntimeJobLine
    (linesBeforeFirstRuntimeJobLine, linesFromRuntimeOutput) = break isRuntimeJobLine outputLines
    cutOffMarker = "<cut off: the app is running>"

-- | A command that is stopped while it prints can end in the middle of an ANSI
-- code, in a different place on every run.
dropUnfinishedAnsiCodeAtEnd :: T.Text -> T.Text
dropUnfinishedAnsiCodeAtEnd output
  | endsInsideAnsiCode = T.dropEnd (T.length lastLine) output
  | otherwise = output
  where
    lastLine = T.takeWhileEnd (/= '\n') output
    endsInsideAnsiCode = "\ESC" `T.isPrefixOf` stripAnsiCodes lastLine

-- | The tools Wasp runs print different things on different runs, and each of
-- their lines carries the job's prefix. Lines of one job that follow each
-- other become a single placeholder line per stream, which keeps the prefix as
-- it was printed.
--
-- A job's stdout and stderr lines mix in a different order on every run, so
-- they count as the same run of lines.
collapseJobOutput :: [T.Text] -> [T.Text]
collapseJobOutput = concatMap collapseIfJobOutput . groupBy ((==) `on` lineJobName) . map parseLine
  where
    parseLine line = (line, parseJobLine line)
    lineJobName = fmap jobName . snd

    collapseIfJobOutput :: [(T.Text, Maybe JobLine)] -> [T.Text]
    collapseIfJobOutput parsedLines = case mapMaybe snd parsedLines of
      [] -> fst <$> parsedLines
      jobLines -> mapMaybe (makePlaceholder jobLines) [JobStdout, JobStderr]

    makePlaceholder :: [JobLine] -> JobStream -> Maybe T.Text
    makePlaceholder jobLines stream =
      (<> "<output>") . rawPrefix <$> find ((== stream) . jobStream) jobLines

-- | Once the app runs, its jobs print at the same time, and the empty lines
-- that set their output apart land in different places on every run. The
-- terminal's echo of Ctrl-C lands in a different place too.
keepOnlyWaspOutput :: [T.Text] -> [T.Text]
keepOnlyWaspOutput = filter isWaspOutput . map dropCtrlCEcho
  where
    isWaspOutput line = not (isJobLine line) && not (isEmptyLine line)
    dropCtrlCEcho = T.replace "^C" ""

containsRuntimeOutputOfWholeApp :: T.Text -> Bool
containsRuntimeOutputOfWholeApp output = all hasPrinted runtimeJobNames
  where
    hasPrinted name = name `elem` printedJobNames
    printedJobNames = jobName <$> mapMaybe parseJobLine (T.lines output)

data JobLine = JobLine
  { -- | The prefix as it was printed, with its ANSI codes.
    rawPrefix :: T.Text,
    jobName :: T.Text,
    jobStream :: JobStream
  }

data JobStream = JobStdout | JobStderr
  deriving (Eq)

isEmptyLine :: T.Text -> Bool
isEmptyLine = T.null . T.strip

isJobLine :: T.Text -> Bool
isJobLine = isJust . parseJobLine

isRuntimeJobLine :: T.Text -> Bool
isRuntimeJobLine = maybe False ((`elem` runtimeJobNames) . jobName) . parseJobLine

runtimeJobNames :: [T.Text]
runtimeJobNames = ["Server", "Client"]

knownJobNames :: [T.Text]
knownJobNames = runtimeJobNames ++ ["Db"]

-- | Recognizes lines like "[ Server ] text" and "[ Server!] text", where the
-- "!" marks the job's stderr.
parseJobLine :: T.Text -> Maybe JobLine
parseJobLine line = do
  afterOpeningBracket <- T.stripPrefix "[" visibleLine
  let (name, afterName) = T.span isAlpha $ T.stripStart afterOpeningBracket
      (stderrFlag, afterStderrFlag) = T.span (== '!') $ T.stripStart afterName
  guard $ name `elem` knownJobNames
  afterPrefix <- T.stripPrefix "] " afterStderrFlag
  let visiblePrefixLength = T.length visibleLine - T.length afterPrefix
  return
    JobLine
      { rawPrefix = fst $ splitAfterVisibleCharacters visiblePrefixLength line,
        jobName = name,
        jobStream = if T.null stderrFlag then JobStdout else JobStderr
      }
  where
    visibleLine = stripAnsiCodes line

stripAnsiCodes :: T.Text -> T.Text
stripAnsiCodes text = case splitLeadingAnsiCode fromAnsiCode of
  Just (_, afterAnsiCode) -> beforeAnsiCode <> stripAnsiCodes afterAnsiCode
  Nothing -> text
  where
    (beforeAnsiCode, fromAnsiCode) = T.breakOn ansiCodeIntroducer text

-- | ANSI codes take up no space on the screen, so they don't count. The ones
-- between the counted characters stay with them.
splitAfterVisibleCharacters :: Int -> T.Text -> (T.Text, T.Text)
splitAfterVisibleCharacters 0 text = ("", text)
splitAfterVisibleCharacters visibleCharactersCount text =
  case splitLeadingAnsiCode text of
    Just (ansiCode, afterAnsiCode) ->
      first (ansiCode <>) $ splitAfterVisibleCharacters visibleCharactersCount afterAnsiCode
    Nothing -> case T.uncons text of
      Just (character, afterCharacter) ->
        first (T.cons character) $ splitAfterVisibleCharacters (visibleCharactersCount - 1) afterCharacter
      Nothing -> ("", "")

-- | Splits off the ANSI code the text starts with, if it starts with one.
splitLeadingAnsiCode :: T.Text -> Maybe (T.Text, T.Text)
splitLeadingAnsiCode text = do
  afterIntroducer <- T.stripPrefix ansiCodeIntroducer text
  let (parameters, fromFinalCharacter) = T.break isAnsiCodeFinalCharacter afterIntroducer
  (finalCharacter, afterAnsiCode) <- T.uncons fromFinalCharacter
  return (ansiCodeIntroducer <> parameters <> T.singleton finalCharacter, afterAnsiCode)
  where
    isAnsiCodeFinalCharacter character = character >= '@' && character <= '~'

ansiCodeIntroducer :: T.Text
ansiCodeIntroducer = "\ESC["

-- | Makes the transcript readable in a diff, without losing what was printed.
makeControlCharactersVisible :: T.Text -> T.Text
makeControlCharactersVisible = T.replace "\ESC" "<ESC>" . T.replace "\r" "<CR>"

mapLinesKeepingLastLineEnding :: ([T.Text] -> [T.Text]) -> T.Text -> T.Text
mapLinesKeepingLastLineEnding mapTextLines text
  | endsWithNewline = T.unlines mappedLines
  | otherwise = T.intercalate "\n" mappedLines
  where
    mappedLines = mapTextLines $ T.lines text
    endsWithNewline = "\n" `T.isSuffixOf` text
