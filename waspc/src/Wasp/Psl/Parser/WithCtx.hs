module Wasp.Psl.Parser.WithCtx
  ( withCtx,
    documentationComment,
  )
where

import Text.Megaparsec
  ( many,
    takeWhileP,
  )
import qualified Text.Megaparsec.Char as C
import Wasp.Psl.Ast.WithCtx
  ( NodeContext (NodeContext, documentationComments),
    WithCtx (WithCtx),
  )
import Wasp.Psl.Comments (DocumentationComment, documentationCommentSymbol)
import Wasp.Psl.Parser.Common (Parser, lexeme)

withCtx :: Parser node -> Parser (WithCtx node)
withCtx nodeParser = do
  leadingComments <- many documentationComment
  node <- nodeParser
  -- TODO: Handle trailing comments properly, right now we fail on them. (#3041)

  return $ WithCtx node (NodeContext {documentationComments = leadingComments})

documentationComment :: Parser DocumentationComment
documentationComment =
  -- Given we're manually parsing on the character level here, we need to wrap
  -- this in a `lexeme`, so that Parsec identifies it as a token and can
  -- correctly handle whitespace around the comment.
  lexeme $
    C.string documentationCommentSymbol
      >> takeWhileP (Just "character") (/= '\n')
