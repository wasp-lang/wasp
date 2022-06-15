module Wasp.LSP.Completion
  ( getCompletionsAtPosition,
  )
where

import Control.Lens ((^.))
import qualified Data.Text as Text
import qualified Language.LSP.Types as LSP
import Wasp.Backend.ConcreteSyntax (SyntaxNode)
import qualified Wasp.Backend.ConcreteSyntax as S
import Wasp.LSP.ServerM
import Wasp.LSP.ServerState

-- TODO: make this smarter
-- for now, just find all declaration names and use them as completion items
getCompletionsAtPosition :: LSP.Position -> ServerM [LSP.CompletionItem]
getCompletionsAtPosition position = do
  src <- gets (^. sourceString)
  declNames <- findDeclNames src 0 . concat <$> gets (^. cst)
  logM $ "[getCompletionsAtPosition] position=" ++ show position ++ " declNames=" ++ show declNames
  return $
    map
      ( \(name, typ) ->
          LSP.CompletionItem
            { _label = Text.pack name,
              _kind = Just LSP.CiVariable,
              _tags = Nothing,
              _detail = Just (Text.pack $ ":: " ++ typ),
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
