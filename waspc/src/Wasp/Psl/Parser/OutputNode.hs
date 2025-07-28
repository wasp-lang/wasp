module Wasp.Psl.Parser.OutputNode
  ( outputNode,
  )
where

import Data.Char (isSpace)
import Data.Maybe (maybeToList)
import Text.Parsec
  ( many,
    optionMaybe,
    satisfy,
    skipMany,
    string,
    try,
  )
import Text.Parsec.String (Parser)
import Wasp.Psl.Ast.OutputNode
  ( DocumentationComment,
    DocumentationComments,
    NodeContext (NodeContext),
    OutputNode (OutputNode),
  )
import Wasp.Psl.Parser.Common (whiteSpace)
import Wasp.Util (trim)

outputNode :: Parser node -> Parser (OutputNode node)
outputNode nodeParser = do
  leadingComments <- freestandingLeadingDocumentationComments

  whiteSpace
  node <- nodeParser
  whiteSpace

  trailingComment <- lineTrailingDocumentationComment

  let allComments = leadingComments ++ maybeToList trailingComment
  return $ OutputNode node (NodeContext allComments)

freestandingLeadingDocumentationComments :: Parser DocumentationComments
freestandingLeadingDocumentationComments = many (try documentationComment)

lineTrailingDocumentationComment :: Parser (Maybe DocumentationComment)
lineTrailingDocumentationComment = do
  skipMany $ satisfy isSpaceExceptNewline
  optionMaybe $ try documentationComment

documentationComment :: Parser DocumentationComment
documentationComment =
  -- Given we're manually parsing on the character level here, we need to wrap
  -- this in a `lexeme`, so that Parsec identifies it as a token and can
  -- correctly handle whitespace around the comment.
  lexeme $
    string "///" >> many (satisfy (/= '\n'))
