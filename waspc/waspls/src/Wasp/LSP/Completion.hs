module Wasp.LSP.Completion
  ( getCompletionsAtPosition,
  )
where

import Control.Lens ((^.))
import Control.Monad.Log.Class (MonadLog (logM))
import Control.Monad.State.Class (MonadState, gets)
import Data.List (sortOn)
import qualified Language.LSP.Types as LSP
import qualified Language.LSP.Types.Lens as LSP
import Wasp.Analyzer.Parser.CST.Traverse (fromSyntaxForest)
import Wasp.LSP.Completions.Common (CompletionContext (..), CompletionProvider)
import qualified Wasp.LSP.Completions.DictKeyCompletion as DictKeyCompletion
import qualified Wasp.LSP.Completions.ExprCompletion as ExprCompletion
import Wasp.LSP.ServerState (ServerState, cst, currentWaspSource)
import Wasp.LSP.Syntax (locationAtOffset, lspPositionToOffset, showNeighborhood)

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
      let location = locationAtOffset offset (fromSyntaxForest syntax)
      logM $ "[getCompletionsAtPosition] position=" ++ show position ++ " offset=" ++ show offset
      logM $ "[getCompletionsAtPosition] neighborhood=\n" ++ showNeighborhood location
      let completionContext = CompletionContext {_src = src, _cst = syntax}
      let runCompletionProvider = \cp -> cp completionContext location
      completionItems <- concat <$> mapM runCompletionProvider completionProviders
      return $ sortOn (^. LSP.label) completionItems

-- | List of all 'CompletionProvider's to use. We break this up into separate
-- modules because the code for each can be pretty unrelated.
completionProviders :: (MonadLog m) => [CompletionProvider m]
completionProviders =
  [ ExprCompletion.getCompletions,
    DictKeyCompletion.getCompletions
  ]
