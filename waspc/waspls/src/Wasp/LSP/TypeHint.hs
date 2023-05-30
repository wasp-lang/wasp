module Wasp.LSP.TypeHint
  ( -- * Type hints for CSTs
    getTypeHint,

    -- * Lower level pieces
    ExprPath (..),
    getExprPath,
    getPathType,
    getExpectedType,
  )
where

import Control.Monad (guard)
import Data.Foldable (find)
import qualified Data.HashMap.Strict as M
import qualified Wasp.Analyzer.Parser.CST as S
import Wasp.Analyzer.Parser.CST.Traverse (Traversal)
import qualified Wasp.Analyzer.Parser.CST.Traverse as T
import qualified Wasp.Analyzer.Parser.Token as Tok
import Wasp.Analyzer.StdTypeDefinitions (stdTypes)
import Wasp.Analyzer.Type (Type)
import qualified Wasp.Analyzer.Type as Type
import Wasp.Analyzer.TypeDefinitions (DeclType (dtBodyType), getDeclType)
import Wasp.LSP.Syntax (lexemeAt)

-- | Get the type that is expected a location in the CST, or 'Nothing' if a
-- type could not be determined.
getTypeHint :: String -> Traversal -> Maybe Type
getTypeHint src location = case getExprPath src location of
  Just path -> getPathType path
  _ -> Nothing

-- | Get the type in 'stdTypes' for the expression path. The path must start
-- with a 'Decl', otherwise 'Nothing' is returned. If the path does not exist in
-- 'stdTypes', 'Nothing' is returned.
getPathType :: [ExprPath] -> Maybe Type
getPathType (Decl declType : path) = getExpectedType declType path
getPathType _ = Nothing

-- | A "path" that a location follows down a type.
--
-- === __Example__
-- For the code
--
-- @
-- app todoApp {
--   auth: {
--     usernameAndPassword: // cursor right here
--   }
-- }
-- @
--
-- The path to the cursor would be @[Decl "app", Key "auth", Key "usernameAndPassword"]@
data ExprPath
  = Decl !String
  | Key !String
  | List
  | Tuple !Int
  deriving (Eq, Show)

-- | Try to get an expression path for the given location, returning 'Nothing'
-- if no path can be determined.
--
-- This function only depends on the syntax to the left of the location, and
-- tries to be as lenient as possible in finding paths.
getExprPath :: String -> Traversal -> Maybe [ExprPath]
getExprPath src location = reverse <$> go location
  where
    -- Recursively travel up the syntax tree, accumlating a path in reverse
    -- order. Each recursion adds at most one new path component.
    go :: Traversal -> Maybe [ExprPath]
    go t = case T.up t of
      Nothing -> Just [] -- Top level of the syntax reached
      Just t' -> case T.kindAt t' of
        S.Program -> Just []
        S.Decl -> do
          typLoc <- find ((== S.DeclType) . T.kindAt) $ T.leftSiblings t
          let typ = lexemeAt src typLoc
          -- Stop recursion after finding a Decl
          return [Decl typ]
        S.DictEntry -> case find ((== S.DictKey) . T.kindAt) $ T.leftSiblings t of
          Just keyLoc -> do
            -- There is a key to the left, so @t@ is the value for that key
            let key = lexemeAt src keyLoc
            t'' <- T.up t'
            guard $ T.kindAt t'' == S.Dict
            (Key key :) <$> go t''
          Nothing -> go t'
        S.List -> (List :) <$> go t' -- Inside a list
        S.Tuple -> do
          -- Inside a tuple, number of non-comma nodes to the left is the tuple
          -- index that @t@ is part of.
          --
          -- We look at non-comma nodes since there is no common @Expr@ node type.
          -- We don't count commas directly because we want to be provide help
          -- even when some commas are missing.
          let nExprsBefore = length $ filter ((/= S.Token Tok.Comma) . T.kindAt) $ T.leftSiblings t
          (Tuple nExprsBefore :) <$> go t'
        _ -> go t' -- Found some other node, just ignore it and continue the tree

-- | @getExpectedType declType exprPathWithoutDecl@ searches 'stdTypeDefs' for
-- a type that coreesponds to the given path, starting at the declaration type
-- given by name.
--
-- === __Example__
-- >>> getExpectedType "app" [Key "auth", Key "methods", Key "usernameAndPassword"]
-- Just (Type.DictType { fields = M.fromList [("configFn", Type.DictOptional { dictEntryType = Type.ExtImportType })] })
getExpectedType :: String -> [ExprPath] -> Maybe Type
getExpectedType declType originalPath = do
  topType <- getDeclType declType stdTypes
  go (dtBodyType topType) originalPath
  where
    -- @go parentType path@ returns the result of following @path@ starting at
    -- @parentType@.
    go :: Type -> [ExprPath] -> Maybe Type
    go typ [] = Just typ
    go _ (Decl _ : _) = Nothing -- Can't follow a decl in the middle of a path
    go typ (Key key : path) =
      case typ of
        Type.DictType fields -> do
          -- Get the type of the field corresponding to the eky
          typ' <- Type.dictEntryType <$> fields M.!? key
          go typ' path
        _ -> Nothing -- Not a dict type, can't use Key here
    go typ (List : path) = case typ of
      Type.ListType typ' -> go typ' path -- Use the inner type of the list
      _ -> Nothing -- Not a list type, can't use List here
    go typ (Tuple idx : path) = case typ of
      Type.TupleType (a, b, cs) -> case idx of
        -- Follow the current type (by index) of the tuple
        0 -> go a path
        1 -> go b path
        n | n <= length cs + 2 -> go (cs !! (n - 2)) path
        _ -> Nothing -- Index is too large for the tuple type
      _ -> Nothing -- Not a tuple type, can't use Tuple here
