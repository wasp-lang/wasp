module Wasp.LSP.ExtImport.Syntax
  ( ExtImportNode (..),
    findExtImportAroundLocation,
    getAllExtImports,
  )
where

import Control.Applicative ((<|>))
import Text.Read (readMaybe)
import Wasp.Analyzer.Parser (ExtImportName (ExtImportField, ExtImportModule))
import qualified Wasp.Analyzer.Parser.CST as S
import Wasp.Analyzer.Parser.CST.Traverse (Traversal)
import qualified Wasp.Analyzer.Parser.CST.Traverse as T
import Wasp.LSP.ExtImport.Path (WaspStyleExtFilePath (WaspStyleExtFilePath))
import Wasp.LSP.Syntax (findChild, lexemeAt)

-- | ExtImport syntax node.
data ExtImportNode = ExtImportNode
  { -- | Location of the 'S.ExtImport' node
    einLocation :: !Traversal,
    einName :: !(Maybe ExtImportName),
    -- | Import path, exactly as it appears in the Wasp source code.
    einPath :: !(Maybe WaspStyleExtFilePath)
  }

-- | Create a 'ExtImportNode' at a location, assuming the location is at a
-- 'S.ExtImport'. It is up to the callee to ensure this.
extImportAtLocation :: String -> Traversal -> ExtImportNode
extImportAtLocation src location =
  let maybeName =
        (ExtImportModule . lexemeAt src <$> findChild S.ExtImportModule location)
          <|> (ExtImportField . lexemeAt src <$> findChild S.ExtImportField location)
      maybePathLexeme = lexemeAt src <$> findChild S.ExtImportPath location
      -- Parse the string lexeme into a Haskell string
      maybeWaspStylePath = WaspStyleExtFilePath <$> (readMaybe =<< maybePathLexeme)
   in ExtImportNode
        { einLocation = location,
          einName = maybeName,
          einPath = maybeWaspStylePath
        }

-- | Look for an 'S.ExtImport' node that is either at the given location or is
-- an ancestor of the location.
--
-- For example, passing in a location pointing to an 'S.ExtImportPath' will
-- return an 'ExtImportNode' for the parent node of that path node.
findExtImportAroundLocation ::
  -- | Wasp source code.
  String ->
  -- | Location to look for external import at.
  Traversal ->
  Maybe ExtImportNode
findExtImportAroundLocation src location = do
  extImport <- findExtImportParent location
  return $ extImportAtLocation src extImport
  where
    findExtImportParent t
      | T.kindAt t == S.ExtImport = Just t
      | otherwise = T.up t >>= findExtImportParent

-- | Finds all external imports in a concrete syntax tree.
getAllExtImports ::
  -- | Wasp source code.
  String ->
  -- | Syntax forest of the entire Wasp file.
  [S.SyntaxNode] ->
  [ExtImportNode]
getAllExtImports src syntax = go $ T.fromSyntaxForest syntax
  where
    go :: Traversal -> [ExtImportNode]
    go t = case T.kindAt t of
      S.ExtImport -> [extImportAtLocation src t]
      _ -> concatMap go $ T.children t
