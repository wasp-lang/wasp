module Wasp.LSP.Completions.DictKeyCompletion
  ( getCompletions,
  )
where

import Control.Lens ((?~), (^.))
import Control.Monad (guard)
import Control.Monad.Log.Class (MonadLog (logM))
import Control.Monad.Reader.Class (MonadReader, asks)
import Data.Bifunctor (Bifunctor (second))
import Data.Foldable (find)
import qualified Data.HashMap.Strict as M
import qualified Data.Text as Text
import qualified Language.LSP.Types as LSP
import qualified Language.LSP.Types.Lens as LSP
import qualified Wasp.Analyzer.Parser.CST as S
import Wasp.Analyzer.Parser.CST.Traverse (Traversal)
import qualified Wasp.Analyzer.Parser.CST.Traverse as T
import qualified Wasp.Analyzer.Parser.Token as Tok
import Wasp.Analyzer.StdTypeDefinitions (stdTypes)
import Wasp.Analyzer.Type (Type)
import qualified Wasp.Analyzer.Type as Type
import Wasp.Analyzer.TypeDefinitions (DeclType (dtBodyType), getDeclType)
import Wasp.LSP.Completions.Common (CompletionContext, CompletionProvider, makeBasicCompletionItem)
import qualified Wasp.LSP.Completions.Common as Ctx
import Wasp.LSP.Syntax (allP, anyP, hasLeft, lexemeAt, parentIs)

getCompletions :: (MonadReader CompletionContext m, MonadLog m) => CompletionProvider m
getCompletions location =
  if not $ isAtDictKeyPlace location
    then do
      logM "[DictKeyCompletion] not at dict key"
      return []
    else do
      logM "[DictKeyCompletion] at dict key"
      src <- asks (^. Ctx.src)
      case getExprPath src location of
        Just wholePath@(Decl declType : path) -> do
          case getExpectedType declType path of
            Nothing -> do
              logM $ "[DictKeyCompletion] no expected type for path " ++ show wholePath
              return []
            Just typ -> do
              logM $ "[DictKeyCompletion] found an expected type for path " ++ show wholePath
              let fields = listFieldsOfType typ
              return $
                map
                  ( \(key, keyType) ->
                      makeBasicCompletionItem (Text.pack key)
                        T.& (LSP.kind ?~ LSP.CiField)
                        T.& (LSP.detail ?~ Text.pack (":: " ++ show keyType))
                        T.& (LSP.insertText ?~ Text.pack (key ++ ": "))
                  )
                  fields
        Just wholePath -> do
          logM $ "[DictKeyCompletion] found expr path without decl " ++ show wholePath
          return []
        Nothing -> do
          logM "[DictKeyCompletion] could not find expr path"
          return []

-- | Checks whether a position in the CST is where a DictKey would be expected.
--
-- The rules for this are:
-- - If parent is a Dict, then yes
-- - If parent is a DictEntry and there are no DictKeys to the left of this node,
--   then yes
-- - Else, no
isAtDictKeyPlace :: Traversal -> Bool
isAtDictKeyPlace =
  anyP
    [ parentIs S.Dict,
      allP [parentIs S.DictEntry, not . hasLeft S.DictKey]
    ]

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
          typ' <- dictEntryType <$> fields M.!? key
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

-- | List the (key, valuetype) pairs for a type. Returns an empty list for
-- everything except a 'Type.DictType'.
listFieldsOfType :: Type -> [(String, Type)]
listFieldsOfType (Type.DictType fields) = map (second dictEntryType) $ M.toList fields
listFieldsOfType _ = []

dictEntryType :: Type.DictEntryType -> Type
dictEntryType (Type.DictOptional typ) = typ
dictEntryType (Type.DictRequired typ) = typ
