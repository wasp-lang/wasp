module Wasp.LSP.TypeInference
  ( -- * Inferred types for CST locations
    inferTypeAtLocation,

    -- * Lower level pieces
    ExprPath,
    ExprPathStep (..),
    findExprPathToLocation,
    findTypeForPath,
  )
where

import Control.Monad (guard)
import Data.Foldable (find)
import qualified Data.HashMap.Strict as M
import qualified Wasp.Analyzer.Parser.CST as S
import Wasp.Analyzer.Parser.CST.Traverse (Traversal)
import qualified Wasp.Analyzer.Parser.CST.Traverse as T
import Wasp.Analyzer.StdTypeDefinitions (stdTypes)
import Wasp.Analyzer.Type (Type)
import qualified Wasp.Analyzer.Type as Type
import Wasp.Analyzer.TypeDefinitions (DeclType (dtBodyType), getDeclType)
import Wasp.LSP.Syntax (lexemeAt)

inferTypeAtLocation :: String -> Traversal -> Maybe Type
inferTypeAtLocation src location = findExprPathToLocation src location >>= findTypeForPath

-- | A "path" through wasp expressions to a certain location.
--
-- === __Example__
-- For the code
--
-- @
-- app todoApp {
--   auth: {
--     usernameAndPassword: |
--                          ^
--   }
-- }
-- @
--
-- The path to the cursor would be @[Decl "app", DictKey "auth", DictKey "usernameAndPassword"]@.
type ExprPath = [ExprPathStep]

data ExprPathStep
  = -- | @Decl declType@. Enter a declaration of type @declType@.
    Decl !String
  | -- | @DictKey key@. Enter a dictionary *and* its key @key@.
    DictKey !String
  | -- | Enter a value inside a list.
    List
  | -- | @Tuple idx@. Enter the @idx@-th value inside of a tuple.
    Tuple !Int
  deriving (Eq, Show)

-- | This function only depends on the syntax to the left of the location, and
-- tries to be as lenient as possible in finding paths.
findExprPathToLocation :: String -> Traversal -> Maybe ExprPath
findExprPathToLocation src location = reverse <$> go location
  where
    -- Recursively travel up the syntax tree, accumlating a path in reverse
    -- order. Each recursion adds at most one new path component.
    go :: Traversal -> Maybe ExprPath
    go t = case T.up t of
      Nothing -> Just [] -- Top level of the syntax reached.
      Just t' -> case T.kindAt t' of
        S.Program -> Just []
        S.Decl -> do
          typLoc <- find ((== S.DeclType) . T.kindAt) $ T.leftSiblings t
          let typ = lexemeAt src typLoc
          -- Stop recursion after finding a Decl
          return [Decl typ]
        S.DictEntry -> case find ((== S.DictKey) . T.kindAt) $ T.leftSiblings t of
          Just keyLoc -> do
            -- There is a key to the left, so @t@ is the value for that key.
            let key = lexemeAt src keyLoc
            t'' <- T.up t'
            guard $ T.kindAt t'' == S.Dict
            (DictKey key :) <$> go t''
          Nothing -> go t'
        S.List -> (List :) <$> go t' -- Inside a list.
        S.Tuple -> do
          -- Inside a tuple, number of expression nodes to the left is the tuple
          -- index that @t@ is part of.
          let nExprsBefore = length $ filter (S.syntaxKindIsExpr . T.kindAt) $ T.leftSiblings t
          (Tuple nExprsBefore :) <$> go t'
        _ -> go t' -- Found some other node, just ignore it and continue the tree.

-- | Get the type in 'stdTypes' for the expression path. The path must start
-- with a 'Decl', otherwise 'Nothing' is returned. If the path's decl does not
-- exist in 'stdTypes', 'Nothing' is returned.
--
-- === __Example__
-- >>> findTypeForPath [Dict "app", Key "auth", Key "methods", Key "usernameAndPassword"]
-- Just (Type.DictType { fields = M.fromList [("configFn", Type.DictOptional { dictEntryType = Type.ExtImportType })] })
findTypeForPath :: ExprPath -> Maybe Type
findTypeForPath (Decl declType : originalPath) = do
  topType <- getDeclType declType stdTypes
  go (dtBodyType topType) originalPath
  where
    -- @go parentType path@ returns the result of following @path@ starting at
    -- @parentType@.
    go :: Type -> ExprPath -> Maybe Type
    go typ [] = Just typ
    go _ (Decl _ : _) = Nothing -- Can't follow a decl in the middle of a path.
    go typ (DictKey key : path) =
      case typ of
        Type.DictType fields -> do
          -- Get the type of the field corresponding to the key.
          typ' <- Type.dictEntryType <$> fields M.!? key
          go typ' path
        _ -> Nothing -- Not a dict type, can't use Key here.
    go typ (List : path) = case typ of
      Type.ListType typ' -> go typ' path -- Use the inner type of the list.
      _ -> Nothing -- Not a list type, can't use List here.
    go typ (Tuple idx : path) = case typ of
      Type.TupleType (a, b, cs) -> case idx of
        -- Follow the current type (by index) of the tuple.
        0 -> go a path
        1 -> go b path
        n | n < length cs + 2 -> go (cs !! (n - 2)) path
        _ -> Nothing -- Index is too large for the tuple type.
      _ -> Nothing -- Not a tuple type, can't use Tuple here.
findTypeForPath _ = Nothing -- Doesn't start with a Decl.
