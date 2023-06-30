{-# LANGUAGE RankNTypes #-}

module Wasp.LSP.CodeActions
  ( getCodeActionsInRange,
  )
where

import Control.Lens ((^.))
import Control.Monad.Log.Class (MonadLog (logM))
import Control.Monad.Reader.Class (MonadReader, asks)
import qualified Data.Text as Text
import qualified Language.LSP.Types as LSP
import Wasp.Analyzer.Parser.CST (SyntaxNode)
import qualified Wasp.Analyzer.Parser.CST as S
import Wasp.Analyzer.Parser.CST.Traverse (Traversal)
import qualified Wasp.Analyzer.Parser.CST.Traverse as T
import Wasp.Analyzer.Parser.SourceSpan (SourceSpan (SourceSpan), spansOverlap)
import Wasp.LSP.ExtImport.Syntax (ExtImportNode (einName, einPath), extImportAtLocation)
import Wasp.LSP.ServerState (ServerState)
import qualified Wasp.LSP.ServerState as State
import Wasp.LSP.Syntax (lspRangeToSpan)

getCodeActionsInRange :: (MonadLog m, MonadReader ServerState m) => LSP.Range -> m [LSP.CodeAction]
getCodeActionsInRange range = do
  logM $ "[getCodeActionsInRange] range=" <> show range
  src <- asks (^. State.currentWaspSource)
  maybeCst <- asks (^. State.cst)
  case maybeCst of
    Nothing -> pure []
    Just syntax -> do
      concat <$> mapM (\f -> f src syntax range) codeActionProviders

codeActionProviders :: (MonadLog m, MonadReader ServerState m) => [String -> [SyntaxNode] -> LSP.Range -> m [LSP.CodeAction]]
codeActionProviders =
  [ tsScaffoldActionProvider
  ]

-- | Provide 'LSP.CodeAction's for missing external imports.
tsScaffoldActionProvider :: (MonadLog m, MonadReader ServerState m) => String -> [SyntaxNode] -> LSP.Range -> m [LSP.CodeAction]
tsScaffoldActionProvider src syntax range = do
  -- VSCode (incorrectly) sends codeAction requests with ranges where the start
  -- position is equal to the end position. This range contains 0 characters,
  -- so we add a character to it.
  --
  -- NOTE: The LSP specification specifies that the end of a range is exclusive:
  -- https://microsoft.github.io/language-server-protocol/specifications/specification-3-16/#range
  let sourceSpan =
        let SourceSpan s e = lspRangeToSpan src range
         in if s == e then SourceSpan s (e + 1) else SourceSpan s e
  let extImports = map (extImportAtLocation src) $ collectExtImportNodesInSpan sourceSpan (T.fromSyntaxForest syntax)
  -- TODO(before merge): create real code actions, and only when there is a diagnostic
  -- that means i also need to connect each extimport to a diagnostic, hmm
  return $
    flip map extImports $ \extImport ->
      LSP.CodeAction
        { _title = Text.pack $ "Create function " <> show (einName extImport) <> " in " <> show (einPath extImport),
          _kind = Just LSP.CodeActionQuickFix,
          _diagnostics = Nothing,
          _isPreferred = Nothing,
          _disabled = Nothing,
          _edit = Nothing,
          _command = Just $ LSP.Command {_title = "Create function", _command = "create.function", _arguments = Nothing},
          _xdata = Nothing
        }
  where
    -- Post-condition: all 'Traversal's returned have @kindAt t == ExtImport@.
    collectExtImportNodesInSpan :: SourceSpan -> Traversal -> [Traversal]
    collectExtImportNodesInSpan sourceSpan t =
      -- Only consider the given traversal if it is within the span.
      if spansOverlap (T.spanAt t) sourceSpan
        then case T.kindAt t of
          S.ExtImport -> [t]
          _ -> concatMap (collectExtImportNodesInSpan sourceSpan) $ T.children t
        else []
