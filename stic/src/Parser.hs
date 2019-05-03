module Parser
    ( parseWasp
    ) where

import Text.Parsec (parse, ParseError)
import Text.Parsec.String (Parser)

import Lexer
import Parser.App
import qualified Wasp


-- | Top level parser, produces Wasp.
waspParser :: Parser Wasp.Wasp
waspParser = do
    -- NOTE(matija): this is the only place we need to use whiteSpace, to skip empty lines
    -- and comments in the beginning of file. All other used parsers are lexeme parsers
    -- so they do it themselves.
    whiteSpace

    -- TODO(matija): extract this into a single parser.
    reserved reservedNameApp
    parsedAppName <- identifier
    parsedAppProperties <- braces $ appProperties

    -- TODO(matija): after we parsed everything, we should do semantic analysis
    -- e.g. check there is only 1 title - if not, throw a meaningful error.

    return $ Wasp.fromApp $ Wasp.App
        { Wasp.appName = parsedAppName
        , Wasp.appTitle = getAppTitle parsedAppProperties
          -- TODO(matija): add favicon.
        }

-- | Top level parser executor.
parseWasp :: String -> Either ParseError Wasp.Wasp
parseWasp wasp = parse waspParser sourceName wasp
  where
    -- NOTE(matija): this is used by Parsec only when reporting errors, but we currently
    -- don't provide source name (e.g. .wasp file name) to this method so leaving it empty
    -- for now.
    sourceName = ""
