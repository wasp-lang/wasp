module Wasp.Psl.Generator.WithCtx
  ( generateWithCtx,
    generateNodeContext,
  )
where

import Wasp.Psl.Ast.WithCtx (NodeContext (NodeContext, documentationComments), WithCtx (WithCtx))
import Wasp.Psl.Generator.Common (PslSource)

generateWithCtx :: (node -> PslSource) -> WithCtx node -> PslSource
generateWithCtx generateNode (WithCtx node context) =
  generateNodeContext context (generateNode node)

generateNodeContext :: NodeContext -> PslSource -> PslSource
generateNodeContext NodeContext {documentationComments = comments} content =
  unlines (map ("///" ++) comments ++ [content])
