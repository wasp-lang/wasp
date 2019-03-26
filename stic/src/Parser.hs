module Parser
    ( parseWasp
    , waspLexer
    ) where

import Text.Parsec
import Text.Parsec.String (Parser)
import Text.Parsec.Language (emptyDef)
import qualified Text.Parsec.Token as Token

import qualified Wasp

-- TODO(matija): maybe separate Lexer in a separate file?
-- Lexer

reservedNameApp :: String
reservedNameApp = "app"

waspLanguageDef :: Token.LanguageDef ()
waspLanguageDef = emptyDef 
    { Token.commentLine = "//"
    , Token.reservedNames = [reservedNameApp]
    , Token.caseSensitive = True
    }

waspLexer :: Token.TokenParser ()
waspLexer = Token.makeTokenParser waspLanguageDef

reserved :: String -> Parser ()
reserved = Token.reserved waspLexer

identifier :: Parser String
identifier = Token.identifier waspLexer

-- Parser

waspParser :: Parser Wasp.Wasp
waspParser = do
    reserved reservedNameApp
    parsedAppName <- identifier
    -- TODO(matija): now parse braces and title, favicon, ...
    return $ Wasp.fromApp $ Wasp.App 
        { Wasp.appName = parsedAppName
        , Wasp.appTitle = "someTitle"
        }

parseWasp :: String -> Either ParseError Wasp.Wasp
parseWasp wasp = parse waspParser sourceName wasp
  where
    -- NOTE(matija): this is used by Parsec only when reporting errors, but we currently
    -- don't provide source name (e.g. .wasp file name) to this method so leaving it empty
    -- for now.
    sourceName = ""
