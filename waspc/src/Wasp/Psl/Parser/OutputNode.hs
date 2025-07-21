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
documentationComment = do
  _ <- string "///"
  line <- many (satisfy (/= '\n'))
  whiteSpace
  return $ trim line

isSpaceExceptNewline :: Char -> Bool
isSpaceExceptNewline c = isSpace c && c /= '\n'
