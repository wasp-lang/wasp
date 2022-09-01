module Control.Syntax.Traverse
  ( -- * Syntax tree traversal

    -- | Library for traversing around a concrete syntax trees. The main
    -- benefits are:
    --
    -- - Easier to find nodes relative to a particular node
    -- - Keeps track of absolute source offset
    Traversal,

    -- * Constructors
    fromSyntax,
    fromSyntaxForest,

    -- * Traversal operations

    -- | See the section on composition functions on how to compose these.
    bottom,
    down,
    up,
    left,
    right,
    next,
    previous,

    -- * Composition functions

    -- | These functions can be used to combine many traversal operators
    -- together in a more readable form.
    --
    -- Using @&@ is recommended so that expressions start with the traversal and
    -- then have the operations. @>=>@ can be used for left-to-right composition
    -- of two operations.
    --
    -- For example, you could write @traversal & pipe (replicate 3 next)@ to move
    -- 3 times to the next position in the traversal.
    --
    -- @&?@ is also exported for the same reason as @&@, but for use with
    -- @Maybe Traversal@.
    (&),
    (&?),
    (>=>),
    pipe,

    -- * Collectors
    kindAt,
    widthAt,
    offsetAt,
    offsetAfter,
    parentKind,
    nodeAt,
    parentNode,
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

import Control.Monad ((>=>))
import Control.Monad.Loops (untilM)
import Data.Foldable (Foldable (foldl'))
import Data.Function ((&))
import Data.List (unfoldr)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Maybe (isJust)
import Wasp.Analyzer.Parser.ConcreteParser.CST (SyntaxKind, SyntaxNode (snodeChildren, snodeKind, snodeWidth))

-- | An in-progress traversal through some tree @f@.
data Traversal = Traversal
  { ancestorLevels :: [TraversalLevel],
    currentLevel :: TraversalLevel
  }
  deriving (Eq, Ord, Show)

data TraversalLevel = TraversalLevel
  { tlCurrentNode :: !SyntaxNode,
    tlCurrentOffset :: !Int,
    tlLeftSiblings :: [SyntaxNode],
    tlRightSiblings :: [SyntaxNode]
  }
  deriving (Eq, Show, Ord)

tLeftSiblings :: Traversal -> [SyntaxNode]
tLeftSiblings t = tlLeftSiblings $ currentLevel t

tRightSiblings :: Traversal -> [SyntaxNode]
tRightSiblings t = tlRightSiblings $ currentLevel t

tChildren :: Traversal -> [SyntaxNode]
tChildren t = snodeChildren $ nodeAt t

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
    { ancestorLevels = [],
      currentLevel = levelFromTraversableTree 0 (t :| ts)
    }

-- | Create a new "TraversalLevel" from a non-empty list of nodes.
levelFromTraversableTree :: Int -> NonEmpty SyntaxNode -> TraversalLevel
levelFromTraversableTree offset (node :| rSiblings) =
  TraversalLevel
    { tlCurrentNode = node,
      tlCurrentOffset = offset,
      tlLeftSiblings = [],
      tlRightSiblings = rSiblings
    }

-- | Left-to-right composition of several traversal operations.
pipe :: [Traversal -> Maybe Traversal] -> (Traversal -> Maybe Traversal)
pipe ops = foldl' (>=>) Just ops

-- | Synonym for @>>=@. Meant to be more visually similar to @&@, since they are
-- used for essentially the same purpose in this library.
(&?) :: Maybe Traversal -> (Traversal -> Maybe Traversal) -> Maybe Traversal
t &? op = t >>= op

-- | Move down the tree to the deepest left-most leaf
bottom :: Traversal -> Traversal
bottom t = maybe t bottom $ t & down

-- | Move down a level in the tree, to the first child of the current position.
down :: Traversal -> Maybe Traversal
down t = case tChildren t of
  [] -> Nothing
  (c : cs) ->
    Just $
      Traversal
        { ancestorLevels = currentLevel t : ancestorLevels t,
          currentLevel = levelFromTraversableTree (offsetAt t) (c :| cs)
        }

-- | Move up a level in the tree, to the parent of the current position.
up :: Traversal -> Maybe Traversal
up t = case ancestorLevels t of
  [] -> Nothing
  (a : as) ->
    Just $
      Traversal
        { ancestorLevels = as,
          currentLevel = a
        }

-- | Move to the sibling left of the current position.
left :: Traversal -> Maybe Traversal
left t = case tLeftSiblings t of
  [] -> Nothing
  (l : ls) ->
    Just $
      t
        { currentLevel =
            TraversalLevel
              { tlCurrentNode = l,
                tlCurrentOffset = offsetAt t - snodeWidth l,
                tlLeftSiblings = ls,
                tlRightSiblings = nodeAt t : tRightSiblings t
              }
        }

-- | Move to the sibling right of the current position.
right :: Traversal -> Maybe Traversal
right t = case tRightSiblings t of
  [] -> Nothing
  (r : rs) ->
    Just $
      t
        { currentLevel =
            TraversalLevel
              { tlCurrentNode = r,
                tlCurrentOffset = offsetAt t + widthAt t,
                tlLeftSiblings = nodeAt t : tLeftSiblings t,
                tlRightSiblings = rs
              }
        }

-- | Move to the next node in the tree.
--
-- The next node is the first childless node encountered after the current
-- position in a left-to-right depth-first-search of the tree.
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
--            start & next
-- @
next :: Traversal -> Maybe Traversal
next t
  | hasChildren t = untilM (not . hasChildren) down t
  | hasAncestors t = case untilM hasRightSiblings up t of
      Nothing -> Nothing
      Just t' -> t' & pipe [right, untilM (not . hasChildren) down]
  | otherwise = Nothing

-- | Move to the previous node in a tree. This is 'next', but moves left instead
-- of right.
previous :: Traversal -> Maybe Traversal
previous t
  | hasChildren t = untilM (not . hasChildren) down t
  | hasAncestors t = case untilM hasLeftSiblings up t of
      Nothing -> Nothing
      Just t' -> t' & pipe [left, untilM (not . hasChildren) $ down >=> rightMostSibling]
  | otherwise = Nothing
  where
    rightMostSibling = untilM (not . hasRightSiblings) right

-- | Get the "SyntaxKind" at the current position.
kindAt :: Traversal -> SyntaxKind
kindAt t = snodeKind $ nodeAt t

-- | Get the width of the current node.
widthAt :: Traversal -> Int
widthAt t = snodeWidth $ nodeAt t

-- | Get the offset of the start of the current node in the source text.
offsetAt :: Traversal -> Int
offsetAt t = tlCurrentOffset (currentLevel t)

-- | Get the offset of the end of the current node in the source text.
offsetAfter :: Traversal -> Int
offsetAfter t = offsetAt t + widthAt t

-- | Get the "SyntaxKind" of the parent of the current position.
--
-- [Property] @'parentKind' t == 'contentAt' (t & 'up')@
parentKind :: Traversal -> Maybe SyntaxKind
parentKind t = kindAt <$> up t

-- | Get the node at the current position.
nodeAt :: Traversal -> SyntaxNode
nodeAt t = tlCurrentNode (currentLevel t)

-- | Get the parent node of the current position.
--
-- [Property] @'parentNode' t == 'nodeAt' (t & 'up')@
parentNode :: Traversal -> Maybe SyntaxNode
parentNode t = nodeAt <$> up t

-- | Get the ancestors of the current position.
ancestors :: Traversal -> [Traversal]
ancestors t = unfoldr step (t & up)
  where
    step Nothing = Nothing
    step (Just t') = Just (t', t' & up)

-- | Get the siblings of the current position (not including the current node).
--
-- [Property] @'siblings' t == 'leftSiblings' t ++ 'rightSiblings' t@
siblings :: Traversal -> [Traversal]
siblings t = leftSiblings t ++ rightSiblings t

-- | Get siblings left of the current position.
leftSiblings :: Traversal -> [Traversal]
leftSiblings t = reverse $ unfoldr step t
  where
    step = left >=> (\x -> return (x, x))

-- | Get siblings right of the current position.
rightSiblings :: Traversal -> [Traversal]
rightSiblings t = unfoldr step t
  where
    step = right >=> (\x -> return (x, x))

-- | Get the children of the current position.
children :: Traversal -> [Traversal]
children t = unfoldr step (t & down)
  where
    step Nothing = Nothing
    step (Just t') = Just (t', t' & right)

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
hasNext t = isJust $ next t

-- | Check if the current position has a previous position. Analogue for 'previous'
-- of 'hasNext'.
hasPrevious :: Traversal -> Bool
hasPrevious t = isJust $ previous t

-- | Check if the current position has at least one parent.
hasAncestors :: Traversal -> Bool
hasAncestors t = not $ null $ ancestorLevels t
