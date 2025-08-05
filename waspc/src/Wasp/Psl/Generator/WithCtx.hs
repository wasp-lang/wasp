module Wasp.Psl.Generator.WithCtx
  ( generateWithCtx,
  )
where

import Wasp.Psl.Ast.WithCtx (NodeContext (NodeContext, documentationComments), WithCtx (WithCtx))
import Wasp.Psl.Comments (documentationCommentSymbol)
import Wasp.Psl.Generator.Common (PslSource)

generateWithCtx :: (node -> PslSource) -> WithCtx node -> PslSource
generateWithCtx generateNode (WithCtx node NodeContext {documentationComments = comments}) =
  commentLines ++ codeLines
  where
    commentLines = unlines $ map (documentationCommentSymbol ++) comments
    codeLines = generateNode node
