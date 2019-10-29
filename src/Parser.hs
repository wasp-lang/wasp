module Parser
    ( parseWasp
    ) where

import Text.Parsec (ParseError, (<|>), many1, eof)
import Text.Parsec.String (Parser)

import Lexer
import Parser.App (app)
import Parser.Page (page)
import Parser.Entity (entity)
import Parser.Common (runWaspParser)
import qualified Wasp

waspElement :: Parser Wasp.WaspElement
waspElement = waspElementApp <|> waspElementPage <|> waspElementEntity

waspElementApp :: Parser Wasp.WaspElement
waspElementApp = Wasp.WaspElementApp <$> app

waspElementPage :: Parser Wasp.WaspElement
waspElementPage = Wasp.WaspElementPage <$> page

waspElementEntity :: Parser Wasp.WaspElement
waspElementEntity = Wasp.WaspElementEntity <$> entity

-- | Top level parser, produces Wasp.
waspParser :: Parser Wasp.Wasp
waspParser = do
    -- NOTE(matija): this is the only place we need to use whiteSpace, to skip empty lines
    -- and comments in the beginning of file. All other used parsers are lexeme parsers
    -- so they do it themselves.
    whiteSpace

    waspElems <- many1 waspElement
    eof

    -- TODO(matija): after we parsed everything, we should do semantic analysis
    -- e.g. check there is only 1 title - if not, throw a meaningful error.
    -- Also, check there is at least one Page defined.

    return $ Wasp.fromWaspElems waspElems

-- | Top level parser executor.
parseWasp :: String -> Either ParseError Wasp.Wasp
parseWasp input = runWaspParser waspParser input
