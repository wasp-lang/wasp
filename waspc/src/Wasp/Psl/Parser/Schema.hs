module Wasp.Psl.Parser.Schema
  ( parsePrismaSchema,
    -- For testing only
    schema,
  )
where

import Control.Arrow (left)
import Text.Megaparsec (choice, eof, errorBundlePretty, sepEndBy)
import qualified Text.Megaparsec as Megaparsec
import qualified Wasp.Psl.Ast.Schema as Psl.Schema
import Wasp.Psl.Parser.Common (Parser, SourceCode)
import Wasp.Psl.Parser.ConfigBlock (configBlock)
import Wasp.Psl.Parser.Enum (enum)
import Wasp.Psl.Parser.Lexer (compulsoryNewline, spaceConsumerNL)
import Wasp.Psl.Parser.Model (model)
import Wasp.Psl.Parser.Type (typeBlock)
import Wasp.Psl.Parser.View (view)
import Wasp.Psl.Parser.WithCtx (withCtx)

parsePrismaSchema :: SourceCode -> Either String Psl.Schema.Schema
parsePrismaSchema = left errorBundlePretty . Megaparsec.parse schema ""

schema :: Parser Psl.Schema.Schema
schema = do
  -- We need to ONLY consume the leading whitespace here, because we use
  -- Megaparsec's lexeme parsers in the sub-parsers (model, enum, configBlock)
  -- which consume the (trailing) whitespace themselves. It's a bit of an
  -- implict behaviour that we need to be aware of.
  spaceConsumerNL
  elements <-
    (`sepEndBy` compulsoryNewline) $
      choice
        [ Psl.Schema.ModelBlock <$> withCtx model,
          Psl.Schema.ViewBlock <$> withCtx view,
          Psl.Schema.TypeBlock <$> withCtx typeBlock,
          Psl.Schema.EnumBlock <$> withCtx enum,
          Psl.Schema.ConfigBlock <$> configBlock
        ]
  -- We want to throw and if there is any source code left after parsing the schema.
  -- If we don't do this, the parser sometimes returns an empty schema when there
  -- are some syntax errors in the schema.
  eof
  return $ Psl.Schema.Schema elements
