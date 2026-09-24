-- | Parses the contents of dotenv files (@.env.server@, @.env.client@, ...).
--
-- Wasp reads these files for its own purposes (e.g. to pass their env vars to
-- deployed apps), but the apps also read them on their own: the server through
-- Node's @dotenv@ and the client through Vite, which uses @dotenv@'s parser
-- too. To make sure Wasp and the apps agree on what a file says, this module
-- is a port of @dotenv@'s @parse@ function
-- (https://github.com/motdotla/dotenv/blob/v16.6.1/lib/main.js), which means:
--
--   * Lines look like @KEY=value@, @KEY: value@, or @export KEY=value@.
--   * Values can be unquoted, or quoted with @'@, @"@ or @`@. Quoted values can
--     span multiple lines. In double-quoted values, @\\n@ and @\\r@ are turned
--     into a newline and a carriage return.
--   * @#@ starts a comment, unless it is inside a quoted value.
--   * Lines that don't fit the format are skipped.
--   * If an env var is defined more than once, the last definition wins.
--   * No expansion: @${VAR}@ and @$(command)@ are kept as they are.
--     (Vite additionally expands @${VAR}@ references, but Wasp doesn't try to
--     replicate that.)
module Wasp.Env.NodeDotenvParser
  ( parseDotEnvContentLikeNodeDotenv,
  )
where

import Control.Monad (void)
import Data.Char (isAsciiLower, isAsciiUpper, isDigit, isSpace)
import Data.Function (on)
import Data.List (dropWhileEnd, nubBy)
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char

type EnvVar = (String, String)

parseDotEnvContentLikeNodeDotenv :: String -> [EnvVar]
parseDotEnvContentLikeNodeDotenv = keepLastDefinitions . scanLines . normalizeNewlines

-- | When an env var is defined more than once, the last definition wins.
-- Env vars are returned in the order in which they were first defined.
keepLastDefinitions :: [EnvVar] -> [EnvVar]
keepLastDefinitions envVars = [(name, lastValueOf name) | (name, _) <- nubBy ((==) `on` fst) envVars]
  where
    lastValueOf name = last [value | (name', value) <- envVars, name' == name]

normalizeNewlines :: String -> String
normalizeNewlines ('\r' : '\n' : rest) = '\n' : normalizeNewlines rest
normalizeNewlines ('\r' : rest) = '\n' : normalizeNewlines rest
normalizeNewlines (c : rest) = c : normalizeNewlines rest
normalizeNewlines [] = []

-- | Tries to match an env var definition at every line start, skipping the
-- lines that don't contain one.
scanLines :: String -> [EnvVar]
scanLines "" = []
scanLines input =
  case runParser' definition (initialState input) of
    (state, Right envVar) -> envVar : scanLines (stateInput state)
    (_, Left _) -> scanLines (skipLine input)
  where
    skipLine = drop 1 . dropWhile (/= '\n')
    initialState s =
      State
        { stateInput = s,
          stateOffset = 0,
          statePosState = PosState s 0 (initialPos "") defaultTabWidth "",
          stateParseErrors = []
        }

type Parser = Parsec Void String

-- | One env var definition: @[export] KEY[=|: ][value][# comment]@, up to
-- (excluding) the end of its last line.
definition :: Parser EnvVar
definition = do
  space
  _ <- optional $ try (string "export" <* some spaceChar)
  name <- some (satisfy isNameChar)
  separator
  rawValue <- value
  return (name, cleanUpValue rawValue)
  where
    isNameChar c = isAsciiUpper c || isAsciiLower c || isDigit c || c `elem` ("_.-" :: String)
    separator = try (space <* char '=') <|> void (char ':' <* spaceChar)

-- | The raw value, as it appears in the file: a quoted value (with the quotes),
-- an unquoted value up to a comment or the end of the line, or nothing.
value :: Parser String
value =
  choice
    [ try (quotedValue <* restOfLine),
      try (unquotedValue <* restOfLine),
      "" <$ restOfLine
    ]
  where
    unquotedValue = some (noneOf ("#\n" :: String))
    restOfLine = hspace <* optional (char '#' <* many (noneOf ("\n" :: String))) <* (eof <|> void (lookAhead (char '\n')))

-- | A value in single, double or backtick quotes, which may span multiple
-- lines and contain the quote character escaped with a backslash.
quotedValue :: Parser String
quotedValue = do
  leadingSpace <- many spaceChar
  quote <- oneOf ("'\"`" :: String)
  content <- quotedContent quote
  return $ leadingSpace ++ [quote] ++ content ++ [quote]

quotedContent :: Char -> Parser String
quotedContent quote = go ""
  where
    go acc =
      choice
        [ char quote >> return (reverse acc),
          try (char '\\' >> char quote) >> go (quote : '\\' : acc),
          anySingle >>= \c -> go (c : acc)
        ]

-- | Mirrors what @dotenv@ does with the raw value: trims it, removes the
-- surrounding quotes, and unescapes newlines if it was double-quoted.
cleanUpValue :: String -> String
cleanUpValue rawValue = unescapeNewlinesIfDoubleQuoted $ removeSurroundingQuotes trimmed
  where
    trimmed = dropWhileEnd isSpace $ dropWhile isSpace rawValue

    removeSurroundingQuotes s@(first : rest@(_ : _))
      | first `elem` quoteChars && last rest == first = init rest
      | otherwise = s
    removeSurroundingQuotes s = s

    quoteChars :: String
    quoteChars = "'\"`"

    unescapeNewlinesIfDoubleQuoted s
      | take 1 trimmed == "\"" = unescapeNewlines s
      | otherwise = s

    unescapeNewlines ('\\' : 'n' : rest) = '\n' : unescapeNewlines rest
    unescapeNewlines ('\\' : 'r' : rest) = '\r' : unescapeNewlines rest
    unescapeNewlines (c : rest) = c : unescapeNewlines rest
    unescapeNewlines [] = []
