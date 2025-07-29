module Wasp.Psl.Parser.WithCtx
  ( withCtx,
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
import Wasp.Psl.Ast.WithCtx
  ( DocumentationComment,
    NodeContext (NodeContext, documentationComments),
    WithCtx (WithCtx),
  )
import Wasp.Psl.Common (documentationCommentSymbol)
import Wasp.Psl.Parser.Common (lexeme)

withCtx :: Parser node -> Parser (WithCtx node)
withCtx nodeParser = do
  leadingComments <- many (try documentationComment)
  node <- nodeParser
  -- TODO: Handle trailing comments properly, right now we fail on them.

  return $ WithCtx node (NodeContext {documentationComments = leadingComments})

documentationComment :: Parser DocumentationComment
documentationComment =
  -- Given we're manually parsing on the character level here, we need to wrap
  -- this in a `lexeme`, so that Parsec identifies it as a token and can
  -- correctly handle whitespace around the comment.
  lexeme $
    string documentationCommentSymbol >> many (satisfy (/= '\n'))
