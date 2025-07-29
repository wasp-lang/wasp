module Wasp.Psl.Generator.WithCtx
  ( generateWithCtx,
  )
where

import Wasp.Psl.Ast.WithCtx (NodeContext (NodeContext, documentationComments), WithCtx (WithCtx))
import Wasp.Psl.Common (documentationCommentSymbol)
import Wasp.Psl.Generator.Common (PslSource)

generateWithCtx :: (node -> PslSource) -> WithCtx node -> PslSource
generateWithCtx generateNode (WithCtx node NodeContext {documentationComments = comments}) =
  unlines $
    map (documentationCommentSymbol ++) comments
      <> lines (generateNode node)
