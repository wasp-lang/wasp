module Wasp.LSP.Completions.DictKeyCompletion
  ( getCompletions,
  )
where

import Control.Lens ((?~), (^.))
import Control.Monad.Log.Class (MonadLog (logM))
import Control.Monad.Reader.Class (MonadReader, asks)
import Data.Bifunctor (Bifunctor (second))
import qualified Data.HashMap.Strict as M
import qualified Data.Text as Text
import qualified Language.LSP.Types as LSP
import qualified Language.LSP.Types.Lens as LSP
import qualified Wasp.Analyzer.Parser.CST as S
import Wasp.Analyzer.Parser.CST.Traverse (Traversal)
import qualified Wasp.Analyzer.Parser.CST.Traverse as T
import Wasp.Analyzer.Type (Type)
import qualified Wasp.Analyzer.Type as Type
import Wasp.LSP.Completions.Common (CompletionContext, CompletionProvider, makeBasicCompletionItem)
import qualified Wasp.LSP.Completions.Common as Ctx
import Wasp.LSP.Syntax (allP, anyP, hasLeft, parentIs)
import Wasp.LSP.TypeHint (getTypeHint)

getCompletions :: (MonadReader CompletionContext m, MonadLog m) => CompletionProvider m
getCompletions location =
  if not $ isAtDictKeyPlace location
    then do
      logM "[DictKeyCompletion] not at dict key"
      return []
    else do
      logM "[DictKeyCompletion] at dict key"
      src <- asks (^. Ctx.src)
      case getTypeHint src location of
        Nothing -> do
          logM "[DictKeyCompletion] no type hint, can not suggest keys"
          return []
        Just typ@(Type.DictType _) -> do
          logM "[DictKeyCompletion] found dict type hint"
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
        Just _ -> do
          logM "[DictKeyCompletion] found non-dict type hint, no keys to suggest"
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

-- | List the (key, valuetype) pairs for a type. Returns an empty list for
-- everything except a 'Type.DictType'.
listFieldsOfType :: Type -> [(String, Type)]
listFieldsOfType (Type.DictType fields) = map (second Type.dictEntryType) $ M.toList fields
listFieldsOfType _ = []
