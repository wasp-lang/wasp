module Parser.App
    ( app
    ) where

import Text.Parsec
import Text.Parsec.String (Parser)

import Lexer
import qualified Wasp
import Parser.Common

-- | A type that describes supported app properties.
data AppProperty
    = Title !String
    | Favicon !String
    deriving (Show, Eq)

-- | Parses supported app properties, expects format "key1: value1, key2: value2, ..."
appProperties :: Parser [AppProperty]
appProperties = commaSep1 $ appPropertyTitle <|> appPropertyFavicon

appPropertyTitle :: Parser AppProperty
appPropertyTitle = Title <$> waspPropertyStringLiteral "title"

appPropertyFavicon :: Parser AppProperty
-- TODO(matija): 'fav.png' currently does not work because of '.'. Support it.
appPropertyFavicon = Favicon <$> waspPropertyStringLiteral "favicon"

-- TODO(matija): unsafe, what if empty list?
getAppTitle :: [AppProperty] -> String
getAppTitle ps = head $ [t | Title t <- ps]

-- | Top level parser, parses App.
app :: Parser Wasp.App
app = do
    (appName, appProps) <- waspElementNameAndProps reservedNameApp appProperties

    return Wasp.App 
        { Wasp.appName = appName
        , Wasp.appTitle = getAppTitle appProps
          -- TODO(matija): add favicon.
        }
