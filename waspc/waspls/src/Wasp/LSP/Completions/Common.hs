{-# LANGUAGE TemplateHaskell #-}

module Wasp.LSP.Completions.Common
  ( CompletionProvider,
    CompletionContext (..),
    src,
    cst,
    makeBasicCompletionItem,
  )
where

import Control.Lens (makeClassy)
import qualified Data.Text as Text
import qualified Language.LSP.Types as LSP
import Wasp.Analyzer.Parser.CST (SyntaxNode)
import Wasp.Analyzer.Parser.CST.Traverse (Traversal)

-- | A function that providers 'LSP.CompletionItems' at a location.
type CompletionProvider m = Traversal -> m [LSP.CompletionItem]

-- | The context given to 'CompletionProvider's, via @MonadReader CompletionContext@.
data CompletionContext = CompletionContext
  { _src :: String,
    _cst :: [SyntaxNode]
  }
  deriving (Eq, Show)

makeClassy 'CompletionContext

-- | Create a completion item containing only a label. Use lenses and 'Control.Lens.(?~)'
-- to set more fields, if desired.
makeBasicCompletionItem :: Text.Text -> LSP.CompletionItem
makeBasicCompletionItem name =
  LSP.CompletionItem
    { _label = name,
      _kind = Nothing,
      _tags = Nothing,
      _detail = Nothing,
      _documentation = Nothing,
      _deprecated = Nothing,
      _preselect = Nothing,
      _sortText = Nothing,
      _filterText = Nothing,
      _insertText = Nothing,
      _insertTextFormat = Nothing,
      _insertTextMode = Nothing,
      _textEdit = Nothing,
      _additionalTextEdits = Nothing,
      _commitCharacters = Nothing,
      _command = Nothing,
      _xdata = Nothing
    }
