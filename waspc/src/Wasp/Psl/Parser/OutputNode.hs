module Wasp.Psl.Parser.OutputNode
  ( outputNode,
    documentationComment,
  )
where

import Text.Parsec
  ( many,
    satisfy,
    string,
    try,
  )
import Text.Parsec.String (Parser)
import Wasp.Psl.Ast.OutputNode
  ( DocumentationComment,
    NodeContext (NodeContext),
    OutputNode (OutputNode),
  )
import Wasp.Psl.Parser.Common (lexeme)

outputNode :: Parser node -> Parser (OutputNode node)
outputNode nodeParser = do
  leadingComments <- many (try documentationComment)
  node <- nodeParser
  -- TODO: Handle trailing comments properly, right now we fail on them.

  return $ OutputNode node (NodeContext leadingComments)

documentationComment :: Parser DocumentationComment
documentationComment =
  -- Given we're manually parsing on the character level here, we need to wrap
  -- this in a `lexeme`, so that Parsec identifies it as a token and can
  -- correctly handle whitespace around the comment.
  lexeme $
    string "///" >> many (satisfy (/= '\n'))
