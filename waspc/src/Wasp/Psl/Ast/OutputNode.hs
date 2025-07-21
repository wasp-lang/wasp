{-# LANGUAGE DeriveDataTypeable #-}

module Wasp.Psl.Ast.OutputNode
  ( OutputNode (..),
    NodeContext (..),
    DocumentationComment,
    DocumentationComments,
    commentedNode,
    getOutputNode,
  )
where

import Data.Data (Data)

-- | A PSL node that defines an output in the generated code. As such, it might
-- have attached context, such as documentation comments, that must be preserved
-- in the AST.
data OutputNode node
  = OutputNode node NodeContext
  deriving (Show, Eq, Data)

data NodeContext = NodeContext
  { documentationComments :: DocumentationComments
  }
  deriving (Show, Eq, Data)

type DocumentationComments = [DocumentationComment]

type DocumentationComment = String

commentedNode :: [String] -> node -> OutputNode node
commentedNode comments node = OutputNode node (NodeContext comments)

getOutputNode :: OutputNode node -> node
getOutputNode (OutputNode node _) = node

instance Functor OutputNode where
  fmap f (OutputNode node context) = OutputNode (f node) context

instance Applicative OutputNode where
  pure node = OutputNode node (NodeContext [])
  OutputNode f context <*> OutputNode node context' =
    OutputNode (f node) (context <> context')

instance Semigroup NodeContext where
  (NodeContext comments1) <> (NodeContext comments2) =
    NodeContext (comments1 <> comments2)

instance Monoid NodeContext where
  mempty = NodeContext []
