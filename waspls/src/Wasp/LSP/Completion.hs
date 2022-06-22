module Wasp.LSP.Completion
  ( getCompletionsAtPosition,
  )
where

import Control.Lens ((^.))
import Control.Syntax.Traverse (fromSyntaxForest)
import qualified Data.Text as Text
import qualified Language.LSP.Types as LSP
import Wasp.Backend.ConcreteSyntax (SyntaxNode)
import qualified Wasp.Backend.ConcreteSyntax as S
import Wasp.LSP.ServerM
import Wasp.LSP.ServerState
import Wasp.LSP.Syntax (isAtExprPlace, positionToOffset, showNeighborhood, toOffset)

getCompletionsAtPosition :: LSP.Position -> ServerM [LSP.CompletionItem]
getCompletionsAtPosition position = do
  src <- gets (^. sourceString)
  mbSyntax <- gets (^. cst)
  case mbSyntax of
    -- If there is no syntax tree, make no completions
    Nothing -> return []
    Just syntax -> do
      let offset = positionToOffset src position
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
getExprCompletions :: String -> [SyntaxNode] -> ServerM [LSP.CompletionItem]
getExprCompletions src syntax = do
  let declNames = findDeclNames src 0 syntax
  logM $ "[getExprCompletions] declnames=" ++ show declNames
  return $
    map
      ( \(name, typ) ->
          LSP.CompletionItem
            { _label = Text.pack name,
              _kind = Just LSP.CiVariable,
              _tags = Nothing,
              _detail = Just (Text.pack $ ":: " ++ typ ++ " (declaration type)"),
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
      )
      declNames

-- | TODO: refactor to use "Traversal"
findDeclNames :: String -> Int -> [SyntaxNode] -> [(String, String)]
findDeclNames _ _ [] = []
findDeclNames src offset (node : rest) = case S.snodeKind node of
  S.Program -> findDeclNames src offset (S.snodeChildren node) ++ findDeclNames src (offset + S.snodeWidth node) rest
  S.Decl ->
    let mbName = findSnode S.DeclName offset (S.snodeChildren node)
        mbTyp = findSnode S.DeclType offset (S.snodeChildren node)
        names = case (mbName, mbTyp) of
          (Just (nameOffset, nameNode), Just (typOffset, typNode)) ->
            let name = lexeme src nameOffset (S.snodeWidth nameNode)
                typ = lexeme src typOffset (S.snodeWidth typNode)
             in [(name, typ)]
          _ -> []
     in names ++ findDeclNames src (offset + S.snodeWidth node) rest
  _ -> findDeclNames src (offset + S.snodeWidth node) rest

findSnode :: S.SyntaxKind -> Int -> [SyntaxNode] -> Maybe (Int, SyntaxNode)
findSnode _ _ [] = Nothing
findSnode k offset (n : ns)
  | S.snodeKind n == k = Just (offset, n)
  | otherwise = findSnode k (offset + S.snodeWidth n) ns

-- | @lexeme src offset width@
lexeme :: String -> Int -> Int -> String
lexeme src offset width = take width $ drop offset src
