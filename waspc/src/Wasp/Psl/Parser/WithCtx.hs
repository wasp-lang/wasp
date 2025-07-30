module Wasp.Psl.Parser.WithCtx
  ( withCtx,
    documentationComment,
  )
where

import Data.Maybe (maybeToList)
import Text.Megaparsec
  ( label,
    many,
    optional,
    takeWhileP,
  )
import qualified Text.Megaparsec.Char as C
import Wasp.Psl.Ast.WithCtx
  ( NodeContext (NodeContext, documentationComments),
    WithCtx (WithCtx),
  )
import Wasp.Psl.Comments (DocumentationComment, documentationCommentSymbol)
import Wasp.Psl.Parser.Common (Parser)
import Wasp.Psl.Parser.Lexer (compulsoryNewline)

withCtx :: Parser node -> Parser (WithCtx node)
withCtx nodeParser = do
  leadingComments <- many (documentationComment <* compulsoryNewline)
  node <- nodeParser
  -- IMPORTANT: We do not consume the trailing newline here! Trailing comments
  -- are only attached to the node if they are on the same line as it.
  trailingComment <- optional documentationComment

  let allComments = leadingComments ++ maybeToList trailingComment
  return $ WithCtx node (NodeContext {documentationComments = allComments})

documentationComment :: Parser DocumentationComment
documentationComment =
  label "documentation comment" $
    C.string documentationCommentSymbol
      >> takeWhileP (Just "character") (/= '\n')
