module Wasp.LSP.Syntax
  ( -- * Syntax

    -- | Module with utilities for working with/looking for patterns in CSTs
    positionToOffset,
    toPosition,
    isAtExprPlace,
  )
where

import Control.Syntax.Traverse
import qualified Language.LSP.Types as J
import qualified Wasp.Backend.ConcreteSyntax as S
import qualified Wasp.Backend.Token as T

positionToOffset :: String -> J.Position -> Int
positionToOffset str (J.Position l c) =
  let linesBefore = take (fromIntegral l - 1) (lines str)
   in sum (map length linesBefore) + fromIntegral c

toPosition :: J.Position -> Traversal -> Traversal
toPosition (J.Position targetLine targetCol) t = go 0 0 t
  where
    go :: J.UInt -> J.UInt -> Traversal -> Traversal
    go l c t'
      | l == targetLine && c >= targetCol = t'
      | l > targetLine = t' -- The target line didn't have enough columns
      -- TODO: uncomment this when hasNext is implemented
      --  | not (hasNext t') = t' -- Source didn't have enough lines
      | kindAt t' == S.Token T.Newline = go (l + 1) c (next t')
      | otherwise = go l (c + fromIntegral (widthAt t')) (next t')

-- | Check whether a position in a CST is somewhere an expression belongs. These
-- locations (as of now) are:
--
-- - Parent is DictEntry, has a DictKey left siblings
-- - Parent is Decl, has DeclType and DeclName left siblings
-- - Parent is a List
-- - Parent is a Tuple
isAtExprPlace :: Traversal -> Bool
isAtExprPlace t =
  (parentIs S.DictEntry && hasLeft S.DictKey)
    || parentIs S.List
    || (parentIs S.Decl && hasLeft S.DeclType && hasLeft S.DeclName)
    || parentIs S.Tuple
  where
    parentIs k = hasAncestors t && (parentKind t == k)
    hasLeft k = k `elem` map S.snodeKind (leftSiblings t)
