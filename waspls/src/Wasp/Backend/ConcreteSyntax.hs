{-# LANGUAGE DeriveGeneric #-}

module Wasp.Backend.ConcreteSyntax
  ( -- * Concrete Syntax
    SyntaxNode (..),
    SyntaxKind (..),
    cstPrettyPrint,
    syntaxKindIsTrivia,
  )
where

import Control.DeepSeq (NFData)
import Data.Aeson (ToJSON)
import Data.List (foldl', intercalate)
import GHC.Generics (Generic)
import Wasp.Backend.Token (TokenKind, tokenKindIsTrivia)

-- | The kind of a "SyntaxNode".
data SyntaxKind
  = Token !TokenKind
  | Program
  | Decl
  | DeclType
  | DeclName
  | Dict
  | DictEntry
  | DictKey
  | List
  | Tuple
  | ExtImport
  | ExtImportModule
  | ExtImportField
  | ExtImportPath
  | String
  | Int
  | Double
  | BoolTrue
  | BoolFalse
  | Var
  | Quoter
  | Error
  deriving (Eq, Ord, Show, Generic)

instance NFData SyntaxKind

instance ToJSON SyntaxKind

-- | A node in a concrete syntax tree. We use such a loosely typed system so
-- that we can store all pieces of source syntax in the tree, including
-- comments, whitespace, and unexpected tokens/characters.
--
-- Later, this CST is processed into an AST, a more strongly typed format that
-- leaves out all of the
data SyntaxNode = SyntaxNode
  { snodeKind :: !SyntaxKind,
    -- | Width of this "SyntaxNode". For nodes with children, this is the sum
    -- of their children's lengths. For nodes without children, this is the
    -- length of the source text covered by the node.
    snodeWidth :: !Int,
    snodeChildren :: [SyntaxNode]
  }
  deriving (Eq, Ord, Show, Generic)

instance NFData SyntaxNode

instance ToJSON SyntaxNode

-- | Pretty print a concrete syntax tree. Shows tree of syntax kinds and their
-- offsets in the source file.
cstPrettyPrint :: SyntaxNode -> String
cstPrettyPrint node = go 0 "" node
  where
    go :: Int -> String -> SyntaxNode -> String
    go offset indent pnode =
      let nodeTxt = indent ++ show (snodeKind pnode) ++ "@" ++ show offset ++ ".." ++ show (offset + snodeWidth pnode)
          childrenTxt =
            fst $
              foldl'
                (\(strs, o) child -> (strs ++ [go o (indent ++ "  ") child], o + snodeWidth child))
                ([], offset)
                (snodeChildren pnode)
       in intercalate "\n" (nodeTxt : childrenTxt)

syntaxKindIsTrivia :: SyntaxKind -> Bool
syntaxKindIsTrivia (Token k) = tokenKindIsTrivia k
syntaxKindIsTrivia _ = False
