module Wasp.LSP.TypeHint
  ( -- * Type hints for CSTs
    getTypeHint,

    -- * Lower level pieces
    ExprPath (..),
    getExprPath,
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

-- | Get the type that is expected a location in the CST, or Nothing if the
-- type could not be found for some reason.
getTypeHint :: String -> Traversal -> Maybe Type
getTypeHint src location = case getExprPath src location of
  Just (Decl declType : path) -> getExpectedType declType path
  _ -> Nothing

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

getExprPath :: String -> Traversal -> Maybe [ExprPath]
getExprPath src location = reverse <$> go location
  where
    go :: Traversal -> Maybe [ExprPath]
    go t = case T.up t of
      Nothing -> Just [] -- Already at the top level
      Just t' -> case T.kindAt t' of
        S.Program -> Just []
        S.Decl -> do
          typLoc <- find ((== S.DeclType) . T.kindAt) $ T.leftSiblings t
          let typ = lexemeAt src typLoc
          (Decl typ :) <$> go t'
        S.DictEntry -> case find ((== S.DictKey) . T.kindAt) $ T.leftSiblings t of
          Just keyLoc -> do
            let key = lexemeAt src keyLoc
            t'' <- T.up t'
            guard $ T.kindAt t'' == S.Dict
            (Key key :) <$> go t''
          Nothing -> go t'
        S.List -> (List :) <$> go t'
        S.Tuple -> do
          let nExprsBefore = length $ filter ((/= S.Token Tok.Comma) . T.kindAt) $ T.leftSiblings t
          (Tuple nExprsBefore :) <$> go t'
        _ -> go t'

-- | @getExpectedType declType exprPathWithoutDecl@
getExpectedType :: String -> [ExprPath] -> Maybe Type
getExpectedType declType originalPath = do
  topType <- getDeclType declType stdTypes
  go (dtBodyType topType) originalPath
  where
    go :: Type -> [ExprPath] -> Maybe Type
    go typ [] = Just typ
    go _ (Decl _ : _) = Nothing
    go typ (Key key : path) =
      case typ of
        Type.DictType fields -> do
          typ' <- Type.dictEntryType <$> fields M.!? key
          go typ' path
        _ -> Nothing
    go typ (List : path) = case typ of
      Type.ListType typ' -> go typ' path
      _ -> Nothing
    go typ (Tuple idx : path) = case typ of
      Type.TupleType (a, b, cs) -> case idx of
        0 -> go a path
        1 -> go b path
        n | n <= length cs + 2 -> go (cs !! (n - 2)) path
        _ -> Nothing
      _ -> Nothing
