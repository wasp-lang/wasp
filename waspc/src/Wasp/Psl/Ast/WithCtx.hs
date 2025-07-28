{-# LANGUAGE DeriveDataTypeable #-}

module Wasp.Psl.Ast.WithCtx
  ( WithCtx (..),
    NodeContext (..),
    DocumentationComment,
    DocumentationComments,
    commentedNode,
    getNode,
  )
where

import Data.Data (Data)

-- | A PSL node that defines an output in the generated code. As such, it might
-- have attached context, such as documentation comments, that must be preserved
-- in the AST.
data WithCtx node
  = WithCtx node NodeContext
  deriving (Show, Eq, Data)

data NodeContext = NodeContext
  { documentationComments :: DocumentationComments
  }
  deriving (Show, Eq, Data)

type DocumentationComments = [DocumentationComment]

type DocumentationComment = String

commentedNode :: [String] -> node -> WithCtx node
commentedNode comments node = WithCtx node (NodeContext comments)

getNode :: WithCtx node -> node
getNode (WithCtx node _) = node

instance Functor WithCtx where
  fmap f (WithCtx node context) = WithCtx (f node) context

instance Applicative WithCtx where
  pure node = WithCtx node (NodeContext [])
  WithCtx f context <*> WithCtx node context' =
    WithCtx (f node) (context <> context')

instance Semigroup NodeContext where
  (NodeContext comments1) <> (NodeContext comments2) =
    NodeContext (comments1 <> comments2)

instance Monoid NodeContext where
  mempty = NodeContext []
