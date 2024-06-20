module Wasp.Psl.Parser.Schema
  ( parsePrismaSchema,
    -- For testing only
    schema,
  )
where

import Text.Parsec
  ( choice,
    eof,
    many,
  )
import qualified Text.Parsec as Parsec
import Text.Parsec.String (Parser)
import qualified Wasp.Psl.Ast.Schema as Psl.Schema
import Wasp.Psl.Parser.Common (SourceCode, whiteSpace)
import Wasp.Psl.Parser.ConfigBlock (configBlock)
import Wasp.Psl.Parser.Enum (enum)
import Wasp.Psl.Parser.Model (model)

parsePrismaSchema :: SourceCode -> Either Parsec.ParseError Psl.Schema.Schema
parsePrismaSchema = Parsec.parse schema ""

schema :: Parser Psl.Schema.Schema
schema = do
  -- We need to ONLY consume the leading whitespace here, because we use
  -- Parsec's lexeme parsers in the sub-parsers (model, enum, configBlock) which consume
  -- the (trailing) whitespace themselves. It's a bit of an implict behaviour
  -- that we need to be aware of.
  whiteSpace
  elements <-
    many $
      choice
        [ Psl.Schema.ModelBlock <$> model,
          Psl.Schema.EnumBlock <$> enum,
          Psl.Schema.ConfigBlock <$> configBlock
        ]
  eof
  return $ Psl.Schema.Schema elements
