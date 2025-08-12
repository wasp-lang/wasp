module Wasp.Psl.Comments
  ( DocumentationComments,
    DocumentationComment,
    documentationCommentSymbol,
  )
where

type DocumentationComments = [DocumentationComment]

type DocumentationComment = String

documentationCommentSymbol :: String
documentationCommentSymbol = "///"
