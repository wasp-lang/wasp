-- | This modules implements general concepts regarding env vars.
-- It is not specific to Wasp in any way.
module Wasp.Env
  ( EnvVar,
    EnvVarName,
    EnvVarValue,
    parseDotEnvFile,
    parseDotEnvContent,
    envVarsToDotEnvContent,
    nubEnvVars,
    formatEnvVarValue,
    findDuplicateEnvVars,
    addEnvVarsUnique,
    addEnvVarsOverride,
  )
where

import Data.Char (isAlphaNum, isAscii, isSpace)
import Data.Function (on)
import Data.List (dropWhileEnd, intercalate, isInfixOf, isPrefixOf, nubBy)
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Text as T
import StrongPath (Abs, File, Path')
import qualified Wasp.Util.IO as IOUtil

type EnvVar = (EnvVarName, EnvVarValue)

type EnvVarName = String

type EnvVarValue = String

-- Reads the specified dotenv file and returns its values.
-- Crashes if file doesn't exist.
parseDotEnvFile :: Path' Abs (File ()) -> IO [EnvVar]
parseDotEnvFile envFile =
  parseDotEnvContent <$> IOUtil.readFile envFile

-- | Parses dotenv content matching Node's `dotenv` package rules (Issue #4896):
-- - Duplicate variable definitions: last definition wins.
-- - `${VAR}` and `$(command)` are kept literally.
-- - Unquoted values may contain spaces (trimmed, up to `#` or newline).
-- - Single (`'`), double (`"`), and backtick (`` ` ``) quotes are supported.
-- - Inside double quotes, `\n` and `\r` are expanded; `\"` is kept as `\"`.
-- - Lines that are not definitions are skipped.
parseDotEnvContent :: String -> [EnvVar]
parseDotEnvContent = keepLastEnvVars . parseLines . normalizeNewlines
  where
    normalizeNewlines :: String -> String
    normalizeNewlines ('\r' : '\n' : rest) = '\n' : normalizeNewlines rest
    normalizeNewlines ('\r' : rest) = '\n' : normalizeNewlines rest
    normalizeNewlines (c : rest) = c : normalizeNewlines rest
    normalizeNewlines [] = []

    keepLastEnvVars :: [EnvVar] -> [EnvVar]
    keepLastEnvVars = reverse . nubEnvVars . reverse

    parseLines :: String -> [EnvVar]
    parseLines [] = []
    parseLines s =
      case parseDefinition s of
        Just (envVar, rest) -> envVar : parseLines rest
        Nothing -> parseLines (dropLine s)

    parseDefinition :: String -> Maybe (EnvVar, String)
    parseDefinition lineStart = do
      let s1 = dropHorizontalSpace lineStart
          s2 = stripOptionalExport s1
          (key, afterKey) = span isKeyChar s2
      if null key
        then Nothing
        else do
          valStart <- stripSeparator afterKey
          case valStart of
            q : afterOpenQuote
              | isQuoteChar q,
                Just (quotedVal, remainingLines) <- parseQuotedValue q afterOpenQuote ->
                  let finalVal = if q == '"' then expandDoubleQuoteNewlines quotedVal else quotedVal
                   in Just ((key, finalVal), remainingLines)
            _ -> Just $ parseUnquotedValue key valStart

    stripOptionalExport :: String -> String
    stripOptionalExport s
      | "export" `isPrefixOf` s,
        c : rest <- drop 6 s,
        isHorizontalSpace c =
          dropHorizontalSpace rest
      | otherwise = s

    stripSeparator :: String -> Maybe String
    stripSeparator s =
      case dropHorizontalSpace s of
        '=' : rest -> Just (dropHorizontalSpace rest)
        _ -> case s of
          ':' : c : rest | isHorizontalSpace c -> Just (dropHorizontalSpace rest)
          _ -> Nothing

    parseQuotedValue :: Char -> String -> Maybe (String, String)
    parseQuotedValue quoteChar = go []
      where
        go _ [] = Nothing
        go acc ('\\' : c : rest)
          | c == quoteChar =
              case go (c : '\\' : acc) rest of
                Just result -> Just result
                Nothing -> closeQuote ('\\' : acc) rest
        go acc (c : rest)
          | c == quoteChar = closeQuote acc rest
          | otherwise = go (c : acc) rest

        closeQuote :: String -> String -> Maybe (String, String)
        closeQuote acc rest =
          let afterClose = dropHorizontalSpace rest
           in case afterClose of
                [] -> Just (reverse acc, [])
                '\n' : nextLines -> Just (reverse acc, nextLines)
                '#' : _ -> Just (reverse acc, dropLine afterClose)
                _ -> Nothing

    parseUnquotedValue :: EnvVarName -> String -> (EnvVar, String)
    parseUnquotedValue key valStart =
      let (currentLine, afterNewline) = break (== '\n') valStart
          remainingLines = case afterNewline of
            '\n' : rest -> rest
            _ -> []
          (beforeComment, _) = break (== '#') currentLine
          trimmed = trimHorizontalSpace beforeComment
          unwrapped = case trimmed of
            q : inner
              | isQuoteChar q,
                not (null inner),
                last inner == q ->
                  let body = init inner
                   in if q == '"' then expandDoubleQuoteNewlines body else body
            _ -> trimmed
       in ((key, unwrapped), remainingLines)

    expandDoubleQuoteNewlines :: String -> String
    expandDoubleQuoteNewlines ('\\' : 'n' : rest) = '\n' : expandDoubleQuoteNewlines rest
    expandDoubleQuoteNewlines ('\\' : 'r' : rest) = '\r' : expandDoubleQuoteNewlines rest
    expandDoubleQuoteNewlines (c : rest) = c : expandDoubleQuoteNewlines rest
    expandDoubleQuoteNewlines [] = []

    dropLine :: String -> String
    dropLine s = case break (== '\n') s of
      (_, '\n' : rest) -> rest
      (_, _) -> []

    isHorizontalSpace :: Char -> Bool
    isHorizontalSpace c = c /= '\n' && c /= '\r' && isJsSpace c

    dropHorizontalSpace :: String -> String
    dropHorizontalSpace = dropWhile isHorizontalSpace

    trimHorizontalSpace :: String -> String
    trimHorizontalSpace = dropWhileEnd isHorizontalSpace . dropHorizontalSpace

    isKeyChar :: Char -> Bool
    isKeyChar c = (isAscii c && isAlphaNum c) || c == '_' || c == '.' || c == '-'

    isQuoteChar :: Char -> Bool
    isQuoteChar c = c == '\'' || c == '"' || c == '`'

-- | Formats environment variables for .env file content.
envVarsToDotEnvContent :: [EnvVar] -> T.Text
envVarsToDotEnvContent vars =
  T.pack $ intercalate "\n" $ map formatEnvVar vars
  where
    formatEnvVar (name, value) = name <> "=" <> formatEnvVarValue value

formatEnvVarValue :: EnvVarValue -> EnvVarValue
formatEnvVarValue rawValue
  | not (needsQuoting rawValue) = rawValue
  | canUseSingleQuotes = concat ["'", rawValue, "'"]
  | canUseBacktickQuotes = concat ["`", rawValue, "`"]
  | otherwise = concat ["\"", escapedForDoubleQuotes, "\""]
  where
    needsQuoting :: String -> Bool
    needsQuoting val =
      any isJsSpace val
        || '#' `elem` val
        || isSurroundedByQuotes val

    isSurroundedByQuotes :: String -> Bool
    isSurroundedByQuotes (q : rest) =
      q `elem` ("'\"`" :: String) && not (null rest) && last rest == q
    isSurroundedByQuotes [] = False

    hasDoubleQuoteConflict :: Bool
    hasDoubleQuoteConflict =
      hasLiteralNewlineEscape
        || ('"' `elem` rawValue && ('#' `elem` rawValue || isSurroundedByQuotes rawValue))

    hasLiteralNewlineEscape :: Bool
    hasLiteralNewlineEscape =
      "\\n" `isInfixOf` rawValue || "\\r" `isInfixOf` rawValue

    hasCarriageReturn :: Bool
    hasCarriageReturn = '\r' `elem` rawValue

    canUseSingleQuotes :: Bool
    canUseSingleQuotes =
      hasDoubleQuoteConflict && not hasCarriageReturn && canWrapInQuote '\'' rawValue

    canUseBacktickQuotes :: Bool
    canUseBacktickQuotes =
      hasDoubleQuoteConflict && not hasCarriageReturn && canWrapInQuote '`' rawValue

    canWrapInQuote :: Char -> String -> Bool
    canWrapInQuote quoteChar = go
      where
        go [] = True
        go ['\\'] = False
        go ('\\' : c : rest)
          | c == quoteChar = go rest
          | otherwise = go (c : rest)
        go (c : rest)
          | c == quoteChar = False
          | otherwise = go rest

    escapedForDoubleQuotes :: String
    escapedForDoubleQuotes = concatMap escapeChar rawValue

    escapeChar :: Char -> String
    escapeChar '\n' = "\\n"
    escapeChar '\r' = "\\r"
    escapeChar c = [c]

isJsSpace :: Char -> Bool
isJsSpace c = isSpace c || c == '\xFEFF' || c == '\x2028' || c == '\x2029'

nubEnvVars :: [EnvVar] -> [EnvVar]
nubEnvVars = nubBy ((==) `on` fst)

findDuplicateEnvVars :: [EnvVar] -> [EnvVar] -> Set EnvVarName
findDuplicateEnvVars existing incoming =
  existingNames `Set.intersection` incomingNames
  where
    existingNames = Set.fromList $ fst <$> existing
    incomingNames = Set.fromList $ fst <$> incoming

-- | Combines existing env vars with incoming ones. If there are duplicates in
-- the incoming env vars, returns a @Left@ of the duplicate env var names.
addEnvVarsUnique :: [EnvVar] -> [EnvVar] -> Either (Set EnvVarName) [EnvVar]
addEnvVarsUnique incoming existing
  | Set.null duplicateNames = Right $ addEnvVarsOverride incoming existing
  | otherwise = Left duplicateNames
  where
    duplicateNames = findDuplicateEnvVars existing incoming

-- | Combines existing env vars with incoming ones. If there are duplicates in
-- the incoming env vars, they override the existing ones.
addEnvVarsOverride :: [EnvVar] -> [EnvVar] -> [EnvVar]
addEnvVarsOverride incoming existing =
  -- Incoming first so that they take priority over existing.
  nubEnvVars $ incoming <> existing
