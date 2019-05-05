module Parser.Page
    ( page
    ) where

import Text.Parsec
import Text.Parsec.String (Parser)

import Lexer
import qualified Wasp
import Parser.Common

data PageProperty
    = Title !String
    | Route !String
    | Content !String
    deriving (Show, Eq)

-- | Parses Page properties, separated by a comma.
pageProperties :: Parser [PageProperty]
pageProperties = commaSep1 $
    pagePropertyTitle
    <|> pagePropertyRoute
    <|> pagePropertyContent

pagePropertyTitle :: Parser PageProperty
pagePropertyTitle = Title <$> waspPropertyStringLiteral "title"

pagePropertyRoute :: Parser PageProperty
pagePropertyRoute = Route <$> waspPropertyStringLiteral "route"

pagePropertyContent :: Parser PageProperty
pagePropertyContent = Content <$> waspPropertyClosure "content"

-- TODO(matija): unsafe, what if empty list?
getPageRoute :: [PageProperty] -> String
-- TODO(matija): we are repeating this pattern. How can we extract it? Consider using
-- Template Haskell, lens and prism.
getPageRoute ps = head $ [r | Route r <- ps]

getPageContent :: [PageProperty] -> String
getPageContent ps = head $ [c | Content c <- ps]

-- | Top level parser, parses Page.
page :: Parser Wasp.Page
page = do
    (pageName, pageProps) <- waspElementNameAndProps reservedNamePage pageProperties

    return Wasp.Page
        { Wasp.pageName = pageName
        , Wasp.pageRoute = getPageRoute pageProps
        , Wasp.pageContent = getPageContent pageProps
        }
