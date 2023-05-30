module Wasp.LSP.Completions.ExprCompletion
  ( getCompletions,
  )
where

import Control.Lens ((?~), (^.))
import Control.Monad.Log.Class (MonadLog (logM))
import Control.Monad.Reader.Class (MonadReader, asks)
import Data.Maybe (maybeToList)
import qualified Data.Text as Text
import qualified Language.LSP.Types as LSP
import qualified Language.LSP.Types.Lens as LSP
import Wasp.Analyzer.Parser.CST (SyntaxNode)
import qualified Wasp.Analyzer.Parser.CST as S
import Wasp.Analyzer.Parser.CST.Traverse
import Wasp.LSP.Completions.Common (CompletionContext, CompletionProvider, makeBasicCompletionItem)
import qualified Wasp.LSP.Completions.Common as Ctx
import Wasp.LSP.Syntax (findChild, isAtExprPlace, lexemeAt)

-- | If the location is at an expression, find declaration names in the file
-- and return them as completion items.
--
-- TODO: include completions for enum variants (use standard type defs from waspc)
getCompletions :: (MonadReader CompletionContext m, MonadLog m) => CompletionProvider m
getCompletions location =
  if not (isAtExprPlace location)
    then do
      logM "[ExprCompletion] not at expression"
      return []
    else do
      logM "[ExprCompletion] at expression"
      src <- asks (^. Ctx.src)
      syntax <- asks (^. Ctx.cst)
      let declNames = findDeclNames src syntax
      logM $ "[ExprCompletion] declnames=" ++ show declNames
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
