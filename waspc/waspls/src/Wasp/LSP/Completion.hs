module Wasp.LSP.Completion
  ( getCompletionsAtPosition,
  )
where

import Control.Lens ((^.))
import Control.Monad.Log.Class (MonadLog (logM))
import Control.Monad.Reader (MonadReader, runReaderT)
import Control.Monad.State.Class (MonadState, gets)
import qualified Language.LSP.Types as LSP
import Wasp.Analyzer.Parser.CST.Traverse
import Wasp.LSP.Completions.Common (CompletionContext (..), CompletionProvider)
import qualified Wasp.LSP.Completions.DictKeyCompletion as DictKeyCompletion
import qualified Wasp.LSP.Completions.ExprCompletion as ExprCompletion
import Wasp.LSP.ServerState (ServerState, cst, currentWaspSource)
import Wasp.LSP.Syntax (lspPositionToOffset, toOffset)

-- | Get the list of completions at a (line, column) position in the source.
getCompletionsAtPosition ::
  (MonadState ServerState m, MonadLog m) =>
  LSP.Position ->
  m [LSP.CompletionItem]
getCompletionsAtPosition position = do
  src <- gets (^. currentWaspSource)
  maybeSyntax <- gets (^. cst)
  case maybeSyntax of
    -- If there is no syntax tree, make no completions
    Nothing -> return []
    Just syntax -> do
      let offset = lspPositionToOffset src position
      -- 'location' is a traversal through the syntax tree that points to 'position'
      let location = toOffset offset (fromSyntaxForest syntax)
      logM $ "[getCompletionsAtPosition] position=" ++ show position ++ " offset=" ++ show offset
      let completionContext = CompletionContext {_src = src, _cst = syntax}
      -- Run all completion providers and concatenate results
      concat
        <$> mapM
          (\m -> runReaderT (m location) completionContext)
          completionProviders

completionProviders :: (MonadReader CompletionContext m, MonadLog m) => [CompletionProvider m]
completionProviders =
  [ ExprCompletion.getCompletions,
    DictKeyCompletion.getCompletions
  ]
