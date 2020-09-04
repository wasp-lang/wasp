module Parser.Page
    ( page
    ) where

import Text.Parsec
import Text.Parsec.String (Parser)
import Data.Maybe (listToMaybe, fromMaybe)

import qualified Wasp.Page as Page
import Wasp.JsImport (JsImport)

import Lexer
import Parser.Common
import qualified Parser.JsImport

data PageProperty
    = Title !String
    | Component !JsImport
    deriving (Show, Eq)

-- | Parses Page properties, separated by a comma.
pageProperties :: Parser [PageProperty]
pageProperties = commaSep1 $
    pagePropertyTitle
    <|> pagePropertyComponent

-- NOTE(matija): this is currently unused?
pagePropertyTitle :: Parser PageProperty
pagePropertyTitle = Title <$> waspPropertyStringLiteral "title"

pagePropertyComponent :: Parser PageProperty
pagePropertyComponent = Component <$> waspProperty "component" Parser.JsImport.jsImport

getPageComponent :: [PageProperty] -> Maybe JsImport
getPageComponent ps = listToMaybe [c | Component c <- ps]

-- | Top level parser, parses Page.
page :: Parser Page.Page
page = do
    (pageName, pageProps) <- waspElementNameAndClosureContent reservedNamePage pageProperties

    return Page.Page
        { Page._name = pageName
        , Page._component = fromMaybe (error "Page component is missing.") (getPageComponent pageProps)
        }
