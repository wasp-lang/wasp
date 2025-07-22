module Wasp.Psl.Generator.OutputNode
  ( generateOutputNodeWith,
    generateNodeContext,
  )
where

import Wasp.Psl.Ast.OutputNode (NodeContext (NodeContext), OutputNode (OutputNode))
import Wasp.Psl.Generator.Common (PslSource)

generateOutputNodeWith :: (node -> PslSource) -> OutputNode node -> PslSource
generateOutputNodeWith generateNode (OutputNode node context) =
  generateNodeContext context (generateNode node)

generateNodeContext :: NodeContext -> PslSource -> PslSource
generateNodeContext (NodeContext comments) content =
  unlines (map ("/// " ++) comments ++ [content])
