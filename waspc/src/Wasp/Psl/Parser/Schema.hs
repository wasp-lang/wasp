module Wasp.Psl.Parser.Schema
  ( parsePrismaSchema,
  )
where

import Text.Parsec
  ( many,
    try,
    (<|>),
  )
import qualified Text.Parsec as Parsec
import Text.Parsec.String (Parser)
import qualified Wasp.Psl.Ast.Model as Model
import Wasp.Psl.Parser.Common
  ( whiteSpace,
  )
import Wasp.Psl.Parser.ConfigBlock (configBlock)
import Wasp.Psl.Parser.Enum (enum)
import Wasp.Psl.Parser.Model (model)

parsePrismaSchema :: String -> Either String Model.Schema
parsePrismaSchema input = case Parsec.parse schema "" input of
  Left err -> Left $ show err
  Right parsedSchema -> Right parsedSchema

-- We want to parse a file with extra content but with some models.
-- Go over the extra content and ignore it. Use <|>. to combine parsers.
schema :: Parser Model.Schema
schema = do
  whiteSpace
  elements <-
    many
      ( try model
          <|> try enum
          <|> try configBlock
      )
  return $ Model.Schema elements
