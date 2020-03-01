module Parser
    ( parseWasp
    ) where

import Text.Parsec (ParseError, (<|>), many1, eof, many)
import Text.Parsec.String (Parser)

import qualified Wasp

import Lexer

import Parser.App (app)
import Parser.Page (page)
import Parser.Entity (entity)
import Parser.Entity.EntityForm (entityForm)
import Parser.Entity.EntityList (entityList)
import Parser.JsImport (jsImport)
import Parser.Button (button)
import Parser.Action (action)
import Parser.Common (runWaspParser)


waspElement :: Parser Wasp.WaspElement
waspElement
    =   waspElementApp
    <|> waspElementPage
    <|> waspElementEntity
    <|> waspElementEntityForm
    <|> waspElementEntityList
    <|> waspElementButton
    <|> waspElementAction

waspElementApp :: Parser Wasp.WaspElement
waspElementApp = Wasp.WaspElementApp <$> app

waspElementButton :: Parser Wasp.WaspElement
waspElementButton = Wasp.WaspElementButton <$> button

waspElementAction :: Parser Wasp.WaspElement
waspElementAction = Wasp.WaspElementAction <$> action

waspElementPage :: Parser Wasp.WaspElement
waspElementPage = Wasp.WaspElementPage <$> page

waspElementEntity :: Parser Wasp.WaspElement
waspElementEntity = Wasp.WaspElementEntity <$> entity

waspElementEntityForm :: Parser Wasp.WaspElement
waspElementEntityForm = Wasp.WaspElementEntityForm <$> entityForm

waspElementEntityList :: Parser Wasp.WaspElement
waspElementEntityList = Wasp.WaspElementEntityList <$> entityList

-- | Top level parser, produces Wasp.
waspParser :: Parser Wasp.Wasp
waspParser = do
    -- NOTE(matija): this is the only place we need to use whiteSpace, to skip empty lines
    -- and comments in the beginning of file. All other used parsers are lexeme parsers
    -- so they do it themselves.
    whiteSpace

    jsImports <- many jsImport

    waspElems <- many1 waspElement

    eof

    -- TODO(matija): after we parsed everything, we should do semantic analysis
    -- e.g. check there is only 1 title - if not, throw a meaningful error.
    -- Also, check there is at least one Page defined.

    return $ (Wasp.fromWaspElems waspElems) `Wasp.setJsImports` jsImports

-- | Top level parser executor.
parseWasp :: String -> Either ParseError Wasp.Wasp
parseWasp input = runWaspParser waspParser input
