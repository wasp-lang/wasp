{-
   Common functions used among Wasp parsers.
-}

module Parser.Common where

import Text.Parsec (ParseError, parse, many, noneOf)
import Text.Parsec.String (Parser)
import qualified Data.Text as T

import Lexer

-- | Runs given wasp parser on a specified input.
runWaspParser :: Parser a -> String -> Either ParseError a
runWaspParser waspParser input = parse waspParser sourceName input
  where
    -- NOTE(matija): this is used by Parsec only when reporting errors, but we currently
    -- don't provide source name (e.g. .wasp file name) to this method so leaving it empty
    -- for now.
    sourceName = ""

 -- | Parses a declaration of wasp element (e.g. App or Page) and its properties.
waspElementNameAndProps
    :: String -- ^ Type of the wasp element (e.g. "app" or "page").
    -> Parser a -- ^ Parser to be used for parsing properties of the wasp element.
    -> Parser (String, a) -- ^ Name of the element and parsed properties.
waspElementNameAndProps elementType properties = do
    -- TODO(matija): should we somehow check if this is a reserved name?
    reserved elementType
    elementName <- identifier
    elementProperties <- braces properties

    return (elementName, elementProperties)

-- | Parses wasp property along with the key, "key: value".
waspProperty :: String -> Parser a -> Parser a
waspProperty key value = symbol key <* colon *> value

-- | Parses wasp property which has a string literal for a value.
-- e.g.: title: "my first app"
waspPropertyStringLiteral :: String -> Parser String
waspPropertyStringLiteral key = waspProperty key stringLiteral

-- | Parses wasp property which has a clojure for a value. Returns content within the
-- clojure.
waspPropertyClosure :: String -> Parser String
waspPropertyClosure key = waspProperty key waspClosure

-- | Parses wasp clojure, which is {...}. Returns content within the clojure.
-- NOTE(matija): currently it is not supported to have clojure within a clojure.
waspClosure :: Parser String
waspClosure = strip <$> (braces $ many $ noneOf "{}")

-- | Removes leading and trailing spaces from a string.
strip :: String -> String
strip = T.unpack . T.strip . T.pack
