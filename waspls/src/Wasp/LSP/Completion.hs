module Wasp.LSP.Completion
  ( getCompletionsAtPosition,
  )
where

import Control.Lens ((?~), (^.))
import Control.Syntax.Traverse
import Data.Maybe (maybeToList)
import qualified Data.Text as Text
import qualified Language.LSP.Types as LSP
import qualified Language.LSP.Types.Lens as LSP
import Wasp.Analyzer.Parser.ConcreteParser.CST (SyntaxNode)
import qualified Wasp.Analyzer.Parser.ConcreteParser.CST as S
import Wasp.LSP.ServerM
import Wasp.LSP.ServerState
import Wasp.LSP.Syntax (findChild, isAtExprPlace, lexemeAt, lspPositionToOffset, showNeighborhood, toOffset)

-- | Get the list of completions at a (line, column) position in the source.
getCompletionsAtPosition :: LSP.Position -> ServerM [LSP.CompletionItem]
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
      logM $ "[getCompletionsAtPosition] neighborhood=\n" ++ showNeighborhood location
      exprCompletions <-
        if isAtExprPlace location
          then do
            logM $ "[getCompletionsAtPosition] position=" ++ show position ++ " atExpr=True"
            getExprCompletions src syntax
          else do
            logM $ "[getCompletionsAtPosition] position=" ++ show position ++ " atExpr=False"
            return []
      let completions = exprCompletions
      return completions

-- | If the location is at an expression, find declaration names in the file
-- and return them as autocomplete suggestions
--
-- TODO: include completions for enum variants (use standard type defs from waspc)
getExprCompletions :: String -> [SyntaxNode] -> ServerM [LSP.CompletionItem]
getExprCompletions src syntax = do
  let declNames = findDeclNames src syntax
  logM $ "[getExprCompletions] declnames=" ++ show declNames
  return $
    map
      ( \(name, typ) ->
          makeBasicCompletionItem (Text.pack name)
            & (LSP.kind ?~ LSP.CiVariable)
            & (LSP.detail ?~ Text.pack (":: " ++ typ ++ " (declaration type)"))
      )
      declNames

-- | Search through the CST and collect all @(declName, declType)@ pairs.
findDeclNames :: String -> [SyntaxNode] -> [(String, String)]
findDeclNames src syntax = traverseForDeclNames $ fromSyntaxForest syntax
  where
    traverseForDeclNames :: Traversal -> [(String, String)]
    traverseForDeclNames t = case kindAt t of
      S.Program -> maybe [] traverseForDeclNames $ down t
      S.Decl ->
        let declNameAndType = maybeToList $ getDeclNameAndType t
         in declNameAndType ++ maybe [] traverseForDeclNames (right t)
      _ -> maybe [] traverseForDeclNames $ right t
    getDeclNameAndType :: Traversal -> Maybe (String, String)
    getDeclNameAndType t = do
      nameT <- findChild S.DeclName t
      typeT <- findChild S.DeclType t
      return (lexemeAt src nameT, lexemeAt src typeT)

-- | Create a completion item containing only a label.
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
