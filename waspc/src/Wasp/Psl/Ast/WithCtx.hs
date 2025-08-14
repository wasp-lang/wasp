{-# LANGUAGE DeriveDataTypeable #-}

module Wasp.Psl.Ast.WithCtx
  ( WithCtx (..),
    NodeContext (..),
    commentedNode,
    getNode,
    empty,
  )
where

import Data.Data (Data)
import Wasp.Psl.Comments (DocumentationComments)

data WithCtx node
  = WithCtx node NodeContext
  deriving (Show, Eq, Data)

data NodeContext = NodeContext
  { documentationComments :: DocumentationComments
  }
  deriving (Show, Eq, Data)

commentedNode :: [String] -> node -> WithCtx node
commentedNode comments node = WithCtx node (NodeContext {documentationComments = comments})

getNode :: WithCtx node -> node
getNode (WithCtx node _) = node

empty :: node -> WithCtx node
empty node = WithCtx node mempty

instance Functor WithCtx where
  fmap f (WithCtx node context) = WithCtx (f node) context

instance Applicative WithCtx where
  pure = empty
  WithCtx f context <*> WithCtx node context' =
    WithCtx (f node) (context <> context')

instance Semigroup NodeContext where
  NodeContext {documentationComments = comments1} <> NodeContext {documentationComments = comments2} =
    NodeContext {documentationComments = comments1 <> comments2}

instance Monoid NodeContext where
  mempty = NodeContext {documentationComments = []}
