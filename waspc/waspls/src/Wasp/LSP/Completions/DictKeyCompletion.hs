module Wasp.LSP.Completions.DictKeyCompletion
  ( getCompletions,
  )
where

import Control.Monad.Log.Class (MonadLog (logM))
import Control.Monad.RWS (MonadReader)
import qualified Wasp.Analyzer.Parser.CST as S
import Wasp.Analyzer.Parser.CST.Traverse (Traversal)
import Wasp.LSP.Completions.Common (CompletionContext, CompletionProvider, makeBasicCompletionItem)
import Wasp.LSP.Syntax (allP, anyP, hasLeft, parentIs)

getCompletions :: (MonadReader CompletionContext m, MonadLog m) => CompletionProvider m
getCompletions location =
  if not $ isAtDictKeyPlace location
    then do
      logM "[DictKeyCompletion] not at dict key"
      return []
    else do
      logM "[DictKeyCompletion] at dict key"
      return [makeBasicCompletionItem "key"]

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
