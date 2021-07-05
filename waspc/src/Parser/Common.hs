{-
   Common functions used among Wasp parsers.
-}

module Parser.Common where

import qualified Data.Text as T
import qualified Lexer as L
import StrongPath (File, Path, Posix, Rel, System)
import qualified StrongPath as SP
import Text.Parsec
  ( ParseError,
    anyChar,
    manyTill,
    parse,
    try,
    unexpected,
  )
import Text.Parsec.String (Parser)

-- | Runs given wasp parser on a specified input.
runWaspParser :: Parser a -> String -> Either ParseError a
runWaspParser waspParser input = parse waspParser sourceName input
  where
    -- NOTE(matija): this is used by Parsec only when reporting errors, but we currently
    -- don't provide source name (e.g. .wasp file name) to this method so leaving it empty
    -- for now.
    sourceName = ""

-- TODO(matija): rename to just "waspElement"?

-- | Parses declaration of a wasp element (e.g. App or Page) and the closure content.
waspElementNameAndClosureContent ::
  -- | Type of the wasp element (e.g. "app" or "page").
  String ->
  -- | Parser to be used for parsing closure content of the wasp element.
  Parser a ->
  -- | Name of the element and parsed closure content.
  Parser (String, a)
waspElementNameAndClosureContent elementType closureContent =
  waspElementNameAndClosure elementType (waspClosure closureContent)

-- | Parses declaration of a wasp element (e.g. App or Page) and the belonging closure.
waspElementNameAndClosure ::
  -- | Element type
  String ->
  -- | Closure parser (needs to parse braces as well, not just the content)
  Parser a ->
  -- | Name of the element and parsed closure content.
  Parser (String, a)
waspElementNameAndClosure elementType closure =
  -- NOTE(matija): It is important to have `try` here because we don't want to consume the
  -- content intended for other parsers.
  -- E.g. if we tried to parse "entity-form" this parser would have been tried first for
  -- "entity" and would consume "entity", so entity-form parser would also fail.
  -- This way when entity parser fails, it will backtrack and allow
  -- entity-form parser to succeed.
  --
  -- TODO(matija): should I push this try higher, to the specific case of entity parser
  -- which is causing the trouble?
  -- This way try will be executed in more cases where it is not neccessary, this
  -- might not be the best for the performance and the clarity of error messages.
  -- On the other hand, it is safer?
  try $ do
    L.reserved elementType
    elementName <- L.identifier
    closureContent <- closure

    return (elementName, closureContent)

-- | Parses declaration of a wasp element linked to an entity.
-- E.g. "entity-form<Task> ..." or "action<Task> ..."
waspElementLinkedToEntity ::
  -- | Type of the linked wasp element (e.g. "entity-form").
  String ->
  -- | Parser to be used for parsing body of the wasp element.
  Parser a ->
  -- | Name of the linked entity, element name and body.
  Parser (String, String, a)
waspElementLinkedToEntity elementType bodyParser = do
  L.reserved elementType
  linkedEntityName <- L.angles L.identifier
  elementName <- L.identifier
  body <- bodyParser
  return (linkedEntityName, elementName, body)

-- | Parses wasp property along with the key, "key: value".
waspProperty :: String -> Parser a -> Parser a
waspProperty key value = L.symbol key <* L.colon *> value

-- | Parses wasp property which has a string literal for a value.
-- e.g.: title: "my first app"
waspPropertyStringLiteral :: String -> Parser String
waspPropertyStringLiteral key = waspProperty key L.stringLiteral

-- | Parses wasp property which has a bool for a value. E.g.: "onEnter: true".
waspPropertyBool :: String -> Parser Bool
waspPropertyBool key = waspProperty key L.bool

-- | Parses wasp property which has an identifier as a key (E.g. field name or filter name).
-- E.g. within an entity-form {} we can set properties for a specific field with a closure of
-- form "FIELD_NAME: {...}" -> FIELD_NAME is then an identifier we need.
waspPropertyWithIdentifierAsKey :: Parser a -> Parser (String, a)
waspPropertyWithIdentifierAsKey valueP = do
  identifier <- L.identifier <* L.colon
  value <- valueP

  return (identifier, value)

-- | Parses wasp closure, which is {...}. Returns parsed content within the closure.
waspClosure :: Parser a -> Parser a
waspClosure = L.braces

-- | Parses wasp property closure where property is an identifier whose value we also
-- need to retrieve.
-- E.g. within an entity-form {} we can set properties for a specific field with a closure of
-- form "FIELD_NAME: {...}" -> FIELD_NAME is then an identifier we need.
waspIdentifierClosure :: Parser a -> Parser (String, a)
waspIdentifierClosure = waspPropertyWithIdentifierAsKey . waspClosure

-- | Parses wasp property which has a closure for a value. Returns parsed content within the
-- closure.
waspPropertyClosure :: String -> Parser a -> Parser a
waspPropertyClosure key closureContent = waspProperty key (waspClosure closureContent)

-- | Parses wasp property which has a jsx closure for a value. Returns the content
-- within the closure.
waspPropertyJsxClosure :: String -> Parser String
waspPropertyJsxClosure key = waspProperty key waspJsxClosure

-- | Parses wasp property which has a css closure for a value. Returns the content
-- within the closure.
waspPropertyCssClosure :: String -> Parser String
waspPropertyCssClosure key = waspProperty key waspCssClosure

-- | Parses wasp jsx closure, which is {=jsx...jsx=}. Returns content within the closure.
waspJsxClosure :: Parser String
waspJsxClosure = waspNamedClosure "jsx"

-- | Parses wasp css closure, which is {=css...css=}. Returns content within the closure.
waspCssClosure :: Parser String
waspCssClosure = waspNamedClosure "css"

-- TODO(martin): write tests and comments.

-- | Parses named wasp closure, which is {=name...name=}. Returns content within the closure.
waspNamedClosure :: String -> Parser String
waspNamedClosure name = do
  _ <- closureStart
  strip <$> manyTill anyChar (try closureEnd)
  where
    closureStart = L.symbol ("{=" ++ name)
    closureEnd = L.symbol (name ++ "=}")

-- | Parses a list of items that can be parsed with given parser.
--   For example, `waspList L.identifier` will parse "[foo, bar, t]" into ["foo", "bar", "t"].
waspList :: Parser a -> Parser [a]
waspList elementParser = L.brackets $ L.commaSep elementParser

-- | Removes leading and trailing spaces from a string.
strip :: String -> String
strip = T.unpack . T.strip . T.pack

-- | Parses relative file path, e.g. "my/file.txt".
relFilePathString :: Parser (Path System (Rel d) (File f))
relFilePathString = do
  path <- L.stringLiteral
  maybe
    (unexpected $ "string \"" ++ path ++ "\": Expected relative file path.")
    return
    (SP.parseRelFile path)

-- | Parses relative posix file path, e.g. "my/file.txt".
relPosixFilePathString :: Parser (Path Posix (Rel d) (File f))
relPosixFilePathString = do
  path <- L.stringLiteral
  maybe
    (unexpected $ "string \"" ++ path ++ "\": Expected relative file path.")
    return
    (SP.parseRelFileP path)
