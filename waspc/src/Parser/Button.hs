module Parser.Button
    ( button
    ) where

import Text.Parsec
import Text.Parsec.String (Parser)

import qualified Lexer as L
import qualified Wasp.Button as Button
import Parser.Common

data Property
    = Label !String
    | OnClick !String -- ^ Name of action to execute on click.
    deriving (Show, Eq)

-- | Parses Button properties, separated by a comma.
properties :: Parser [Property]
properties = L.commaSep1 $
    propLabel
    <|> propOnClickActionName

propLabel :: Parser Property
propLabel = Label <$> waspPropertyStringLiteral "label"

propOnClickActionName :: Parser Property
propOnClickActionName = OnClick <$> waspProperty "onClick" L.identifier

getLabel :: [Property] -> String
getLabel ps = head $ [c | Label c <- ps]

getOnClickActionName :: [Property] -> Maybe String
getOnClickActionName ps = let actions = [a | OnClick a <- ps]
                          in if null actions then Nothing else Just (head actions)

button :: Parser Button.Button
button = do
    (buttonName, buttonProps) <- waspElementNameAndClosure L.reservedNameButton properties

    return Button.Button
        { Button._name = buttonName
        , Button._label = getLabel buttonProps
        , Button._onClickActionName = getOnClickActionName buttonProps
        }
