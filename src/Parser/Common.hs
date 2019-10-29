{-
   Common functions used among Wasp parsers.
-}

module Parser.Common where

import Text.Parsec (ParseError, parse, many, noneOf, anyChar, manyTill, try)
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

 -- | Parses a declaration of wasp element (e.g. App or Page) and the closure content.
waspElementNameAndClosure
    :: String -- ^ Type of the wasp element (e.g. "app" or "page").
    -> Parser a -- ^ Parser to be used for parsing closure content of the wasp element.
    -> Parser (String, a) -- ^ Name of the element and parsed closure content.
waspElementNameAndClosure elementType closure = do
    -- TODO(matija): should we somehow check if this is a reserved name?
    reserved elementType
    elementName <- identifier
    elementClosureContent <- braces closure

    return (elementName, elementClosureContent)

-- | Parses wasp property along with the key, "key: value".
waspProperty :: String -> Parser a -> Parser a
waspProperty key value = symbol key <* colon *> value

-- | Parses wasp property which has a string literal for a value.
-- e.g.: title: "my first app"
waspPropertyStringLiteral :: String -> Parser String
waspPropertyStringLiteral key = waspProperty key stringLiteral

-- | Parses wasp property which has a closure for a value. Returns the content within the
-- closure.
waspPropertyClosure :: String -> Parser String
waspPropertyClosure key = waspProperty key waspClosure

-- | Parses wasp property which has a jsx closure for a value. Returns the content
-- within the closure.
waspPropertyJsxClosure :: String -> Parser String
waspPropertyJsxClosure key = waspProperty key waspJsxClosure

-- | Parses wasp property which has a css closure for a value. Returns the content
-- within the closure.
waspPropertyCssClosure :: String -> Parser String
waspPropertyCssClosure key = waspProperty key waspCssClosure

-- | Parses wasp clojure, which is {...}. Returns content within the closure.
-- NOTE(matija): currently it is not supported to have clojure within a closure.
waspClosure :: Parser String
waspClosure = strip <$> (braces $ many $ noneOf "{}")

-- | Parses wasp jsx closure, which is {=jsx...jsx=}. Returns content within the closure.
waspJsxClosure :: Parser String
waspJsxClosure = waspNamedClosure "jsx"

-- | Parses wasp css closure, which is {=css...css=}. Returns content within the closure.
waspCssClosure :: Parser String
waspCssClosure = waspNamedClosure "css"

-- TODO(martin): write tests and comments.
-- | Parses wasp css closure, which is {=name...name=}. Returns content within the closure.
waspNamedClosure :: String -> Parser String
waspNamedClosure name = do
    _ <- closureStart
    strip <$> (manyTill anyChar (try closureEnd))
  where
      closureStart = symbol ("{=" ++ name)
      closureEnd = symbol (name ++ "=}")

-- | Removes leading and trailing spaces from a string.
strip :: String -> String
strip = T.unpack . T.strip . T.pack
