module Parser.Page
    ( page
    ) where

import Text.Parsec
import Text.Parsec.String (Parser)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Maybe (listToMaybe)

import Lexer
import qualified Wasp.Page as Page
import Parser.Common

data PageProperty
    = Title !String
    | Route !String
    | Content !String
    | Style !Text
    deriving (Show, Eq)

-- | Parses Page properties, separated by a comma.
pageProperties :: Parser [PageProperty]
pageProperties = commaSep1 $
    pagePropertyTitle
    <|> pagePropertyRoute
    <|> pagePropertyContent
    <|> pagePropertyStyle

pagePropertyTitle :: Parser PageProperty
pagePropertyTitle = Title <$> waspPropertyStringLiteral "title"

pagePropertyRoute :: Parser PageProperty
pagePropertyRoute = Route <$> waspPropertyStringLiteral "route"

pagePropertyContent :: Parser PageProperty
pagePropertyContent = Content <$> waspPropertyJsxClosure "content"

pagePropertyStyle :: Parser PageProperty
pagePropertyStyle = (Style . Text.pack) <$> waspPropertyCssClosure "style"

-- TODO(matija): unsafe, what if empty list?
getPageRoute :: [PageProperty] -> String
-- TODO(matija): we are repeating this pattern. How can we extract it? Consider using
-- Template Haskell, lens and prism.
getPageRoute ps = head $ [r | Route r <- ps]

getPageContent :: [PageProperty] -> String
getPageContent ps = head $ [c | Content c <- ps]

getPageStyle :: [PageProperty] -> Maybe Text
getPageStyle ps = listToMaybe [s | Style s <- ps]

-- | Top level parser, parses Page.
page :: Parser Page.Page
page = do
    (pageName, pageProps) <- waspElementNameAndClosure reservedNamePage pageProperties

    return Page.Page
        { Page.pageName = pageName
        , Page.pageRoute = getPageRoute pageProps
        , Page.pageContent = getPageContent pageProps
        , Page.pageStyle = getPageStyle pageProps
        }
