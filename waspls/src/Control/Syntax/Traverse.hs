{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
-- For Eq, Ord, Show instances on Traversal
{-# LANGUAGE UndecidableInstances #-}

module Control.Syntax.Traverse
  ( -- * Generalized tree traversals

    -- | Library for traversing around trees isomorphic to rose trees.
    --
    -- A design choice was made in this module to prefer partial functions with
    -- explicit invariants that must be checked by users of the library. This
    -- was done to reduce code noise in the use of the API.
    --
    -- TODO: Is this design choice justified in the context of how it is used?
    -- Does use generally assume invariants are true or does it check them each
    -- time?
    Traversal,

    -- * Constructors
    fromSyntax,
    fromSyntaxForest,

    -- * Traversal operations

    -- | The operators '(&)' and '(.>)' are exported to allow for a slightly
    -- more natural order in using traversal operations. For example, to
    -- get to the 3rd child from the left down 2 levels in a tree you can do
    --
    -- >>> traversal & down & down & right & right
    --
    -- Or
    --
    -- >>> traversal & down .> down .> right .>
    --
    -- Which is equivalent to:
    --
    -- >>> right $ right $ down $ down $ traversal
    --
    -- '(&)' is reverse function application ('($)'), '(.>)' is reverse function
    -- composition ('(.)').
    (&),
    (.>),
    down,
    up,
    left,
    right,
    next,
    back,

    -- * Collectors
    kindAt,
    widthAt,
    offsetAt,
    parentKind,
    nodeAt,
    parentOf,
    ancestors,
    siblings,
    leftSiblings,
    rightSiblings,
    children,

    -- * Predicates
    hasChildren,
    hasLeftSiblings,
    hasRightSiblings,
    hasNext,
    hasPrevious,
    hasAncestors,
  )
where

import Data.Function ((&))
import Wasp.Backend.ConcreteSyntax (SyntaxKind, SyntaxNode (SyntaxNode, snodeChildren, snodeKind, snodeWidth))

-- | An in-progress traversal through some tree @f@.
data Traversal = Traversal
  { tAncestors :: [TraversalLevel],
    tCurrent :: TraversalLevel
  }
  deriving (Eq, Ord, Show)

data TraversalLevel = TraversalLevel
  { tlKind :: !SyntaxKind,
    tlWidth :: !Int,
    tlOffset :: !Int,
    tlChildren :: [SyntaxNode],
    tlLeftSiblings :: [SyntaxNode],
    tlRightSiblings :: [SyntaxNode]
  }
  deriving (Eq, Show, Ord)

-- | Create a new "Traversal" from a "SyntaxNode", starting at the root.
fromSyntax :: SyntaxNode -> Traversal
fromSyntax t = fromSyntaxForest [t]

-- | Create a new "Traversal" from a forest of "TraversableTree"s, starting
-- at the first tree in the list.
--
-- This function is not total. Invariant: @not (null forest)@
fromSyntaxForest :: [SyntaxNode] -> Traversal
fromSyntaxForest [] = error "Control.Tree.Traversal.fromTraversableForest on empty list"
fromSyntaxForest (t : ts) =
  Traversal
    { tAncestors = [],
      tCurrent = levelFromTraversableTree 0 t ts
    }

-- | Create a new "TraversalLevel" from a node and its right siblings.
levelFromTraversableTree :: Int -> SyntaxNode -> [SyntaxNode] -> TraversalLevel
levelFromTraversableTree offset node rSiblings =
  TraversalLevel
    { tlKind = snodeKind node,
      tlWidth = snodeWidth node,
      tlOffset = offset,
      tlChildren = snodeChildren node,
      tlLeftSiblings = [],
      tlRightSiblings = rSiblings
    }

-- | Reverse function composition.
--
-- @f .> g == g . f@
infixl 2 .>

(.>) :: (a -> b) -> (b -> c) -> a -> c
f .> g = g . f

-- | Move down a level in the tree, to the first child of the current position.
--
-- This function is not total. Invariant: @'hasChildren' t == True@
down :: Traversal -> Traversal
down t = case tlChildren (tCurrent t) of
  [] -> error "Control.Tree.Traversal.down on leaf"
  (c : cs) ->
    Traversal
      { tAncestors = tCurrent t : tAncestors t,
        tCurrent = levelFromTraversableTree (offsetAt t) c cs
      }

-- | Move up a level in the tree, to the parent of the current position.
--
-- This function is not total. Invariant: @'hasAncestors' t == True@
up :: Traversal -> Traversal
up t = case tAncestors t of
  [] -> error "Control.Tree.Traversal.up on root"
  (a : as) ->
    Traversal
      { tAncestors = as,
        tCurrent = a
      }

-- | Move to the sibling left of the current position.
--
-- This function is not total. Invariant: @'hasLeftSiblings' t == True@
left :: Traversal -> Traversal
left t = case leftSiblings t of
  [] -> error "Control.Tree.Traversal.left with no left siblings"
  (l : ls) ->
    t
      { tCurrent =
          TraversalLevel
            { tlKind = snodeKind l,
              tlWidth = snodeWidth l,
              tlOffset = offsetAt t - snodeWidth l,
              tlChildren = snodeChildren l,
              tlLeftSiblings = ls,
              tlRightSiblings = nodeAt t : rightSiblings t
            }
      }

-- | Move to the sibling right of the current position.
--
-- This function is not total. Invariant: @'hasRightSiblings' t == True@
right :: Traversal -> Traversal
right t = case rightSiblings t of
  [] -> error "Control.Tree.Traversal.right with no right siblings"
  (r : rs) ->
    t
      { tCurrent =
          TraversalLevel
            { tlKind = snodeKind r,
              tlWidth = snodeWidth r,
              tlOffset = offsetAt t + widthAt t,
              tlChildren = snodeChildren r,
              tlLeftSiblings = nodeAt t : leftSiblings t,
              tlRightSiblings = rs
            }
      }

-- | Move to the next node in the tree.
--
-- The next node is the first childless node encountered after the current
-- position in a left-to-right depth-first-search of the tree.
--
-- This function is not total. Invariant: @'hasNext' t == True@.
--
-- __Examples:__
--
-- Moving to the child of the current node. This looks slightly surprising (it
-- is moving to the left), but this is just an artifact of the diagram. All
-- children are considered to occur after their parent.
--
-- @
--     ┌───B───┐
--     │       │
--   ┌─C─┐   ┌─F─┐
--   │ ▲ │   │   │
-- ┌►D │ E   G   H
-- │   │
-- │ start
-- │
-- └─start & next
-- @
--
-- Even though @J@ is a level higher than @H@, it is the next node.
--
-- @
--       ┌───────A────┐
--       │            │
--   ┌───B───┐     ┌──I──┐
--   │       │     │     │
-- ┌─C─┐   ┌─F─┐   J   ┌─K─┐
-- │   │   │   │   ▲   │   │
-- D   E   G   H   │   L   M
--             ▲   │
--           start │
--                 │
--           start & next
-- @
next :: Traversal -> Traversal
next t
  | hasChildren t = until (not . hasChildren) down t
  | hasAncestors t = case until (\t' -> hasRightSiblings t' || not (hasAncestors t')) up t of
    t'
      | not (hasAncestors t') -> error "Control.Tree.Traversal.next with no next nodes"
      | otherwise -> until (not . hasChildren) down (t' & right)
  | otherwise = error "Control.Tree.Traversal.next with no next nodes"

-- | Move to the previous node in a tree. This is 'next', but moves left instead
-- of right.
--
-- This function is not total. Invariant: @'hasPrevious' t == True@
back :: Traversal -> Traversal
back t
  | hasChildren t = until (not . hasChildren) down t
  | hasAncestors t = case until (\t' -> hasLeftSiblings t' || not (hasAncestors t')) up t of
    t'
      | not (hasAncestors t') -> error "Control.Tree.Traversal.back with no previous nodes"
      | otherwise -> until (not . hasChildren) (down .> until (not . hasRightSiblings) right) (t' & left)
  | otherwise = error "Control.Tree.Traversal.back with no previous nodes"

-- | Get the "SyntaxKind" at the current position.
kindAt :: Traversal -> SyntaxKind
kindAt t = tlKind (tCurrent t)

-- | Get the width of the current node.
widthAt :: Traversal -> Int
widthAt t = tlWidth (tCurrent t)

-- | Get the offset of the start of the current node in the source text.
offsetAt :: Traversal -> Int
offsetAt t = tlOffset (tCurrent t)

-- | Get the "SyntaxKind" of the parent of the current position.
--
-- [Property] @'parentKind' t == 'contentAt' (t & 'up')@
--
-- This function is not total. Invariant: @'hasAncestors' t == True@
parentKind :: Traversal -> SyntaxKind
parentKind t = kindAt $ up t

-- | Get the node at the current position.
nodeAt :: Traversal -> SyntaxNode
nodeAt t =
  SyntaxNode
    { snodeKind = kindAt t,
      snodeWidth = widthAt t,
      snodeChildren = children t
    }

-- | Get the parent node of the current position.
--
-- [Property] @'parentOf' t == 'nodeAt' (t & 'up')@
--
-- This function is not total. Invariant: @'hasAncestors' t == True@
parentOf :: Traversal -> SyntaxNode
parentOf t = nodeAt $ up t

-- | Get the ancestor nodes of the current position.
ancestors :: Traversal -> [SyntaxNode]
ancestors t
  | hasAncestors t = nodeAt t : ancestors (t & up)
  | otherwise = []

-- | Get the siblings of the current position (not including the current node).
--
-- [Property] @'siblings' t == 'leftSiblings' t ++ 'rightSiblings' t@
siblings :: Traversal -> [SyntaxNode]
siblings t = leftSiblings t ++ rightSiblings t

-- | Get siblings left of the current position.
leftSiblings :: Traversal -> [SyntaxNode]
leftSiblings t = tlLeftSiblings (tCurrent t)

-- | Get siblings right of the current position.
rightSiblings :: Traversal -> [SyntaxNode]
rightSiblings t = tlRightSiblings (tCurrent t)

-- | Get the children of the current position.
children :: Traversal -> [SyntaxNode]
children t = tlChildren (tCurrent t)

-- | Check if the current position has children.
hasChildren :: Traversal -> Bool
hasChildren t = not $ null $ children t

-- | Check if the current position has siblings to the left.
hasLeftSiblings :: Traversal -> Bool
hasLeftSiblings t = not $ null $ leftSiblings t

-- | Check if the current position has siblings to the right.
hasRightSiblings :: Traversal -> Bool
hasRightSiblings t = not $ null $ rightSiblings t

-- | Check if the current position has a next position. See the documentation for
-- 'next' for a definition of what this means.
hasNext :: Traversal -> Bool
hasNext t = undefined

-- | Check if the current position has a previous position. Analogue for 'back'
-- of 'hasNext'.
hasPrevious :: Traversal -> Bool
hasPrevious t = undefined

-- | Check if the current position has at least one parent.
hasAncestors :: Traversal -> Bool
hasAncestors t = not $ null $ tAncestors t
