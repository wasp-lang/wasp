module Parser.Page
    ( page
    ) where

import Text.Parsec
import Text.Parsec.String (Parser)
import Data.Maybe (listToMaybe)

import Lexer
import qualified Wasp.Page as Page
import qualified Wasp.Style
import Parser.Common
import qualified Parser.Style

data PageProperty
    = Title !String
    | Content !String
    | Style !Wasp.Style.Style
    deriving (Show, Eq)

-- | Parses Page properties, separated by a comma.
pageProperties :: Parser [PageProperty]
pageProperties = commaSep1 $
    pagePropertyTitle
    <|> pagePropertyContent
    <|> pagePropertyStyle

pagePropertyTitle :: Parser PageProperty
pagePropertyTitle = Title <$> waspPropertyStringLiteral "title"

pagePropertyContent :: Parser PageProperty
pagePropertyContent = Content <$> waspPropertyJsxClosure "content"

pagePropertyStyle :: Parser PageProperty
pagePropertyStyle = Style <$> waspProperty "style" Parser.Style.style

getPageContent :: [PageProperty] -> String
getPageContent ps = head $ [c | Content c <- ps]

getPageStyle :: [PageProperty] -> Maybe Wasp.Style.Style
getPageStyle ps = listToMaybe [s | Style s <- ps]

-- | Top level parser, parses Page.
page :: Parser Page.Page
page = do
    (pageName, pageProps) <- waspElementNameAndClosure reservedNamePage pageProperties

    return Page.Page
        { Page.pageName = pageName
        , Page.pageContent = getPageContent pageProps
        , Page.pageStyle = getPageStyle pageProps
        }
