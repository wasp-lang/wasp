module Parser.App where

import Text.Parsec
import Text.Parsec.String (Parser)

import Lexer

-- | A type that describes supported app properties.
data AppProperty
    = Title String
    | Favicon String
    deriving (Show, Eq)

-- | Parses supported app properties, expects format "key1: value1, key2: value2, ..."
appProperties :: Parser [AppProperty]
appProperties = commaSep1 $ appPropertyTitle <|> appPropertyFavicon

appPropertyTitle :: Parser AppProperty
appPropertyTitle = Title <$> appPropertyWithKey "title"

appPropertyFavicon :: Parser AppProperty
-- TODO(matija): 'fav.png' currently does not work because of '.'. Support it.
appPropertyFavicon = Favicon <$> appPropertyWithKey "favicon"

-- | Helper function, parses a key/value pair. E.g. 'title: "Some title"'.
appPropertyWithKey :: String -> Parser String
appPropertyWithKey key = symbol key <* colon *> stringLiteral

-- TODO(matija): unsafe, what if empty list?
getAppTitle :: [AppProperty] -> String
getAppTitle ps = head $ [t | Title t <- ps]
