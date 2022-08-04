{-# LANGUAGE DeriveGeneric #-}

module Wasp.Analyzer.Parser.TokenSet
  ( -- * TokenSet

    -- | A set of "TokenKind"s and possibly EOF (which is not a "TokenKind").
    --
    -- In functions on a "TokenSet", a @Maybe "TokenKind"@ is either @Nothing@,
    -- representing EOF, or @Just k@, representing the kind @k@.
    TokenSet,

    -- * Membership predicates
    member,
    eofMember,
    kindMember,
    Wasp.Analyzer.Parser.TokenSet.null,

    -- * Operations
    insert,
    insertKind,
    insertEof,
    union,
    intersection,

    -- * Constructors
    empty,
    singleton,
    fromEOF,
    fromKind,
    fromList,

    -- * Destructors
    toList,
    showTokenSet,
  )
where

import Control.DeepSeq (NFData)
import Data.List (intercalate)
import Data.Set (Set)
import qualified Data.Set as Set
import GHC.Generics (Generic)
import Wasp.Analyzer.Parser.Token (TokenKind, showTokenKind)

data TokenSet = TokenSet
  { -- | Check if EOF is part of the "TokenSet"
    eofMember :: !Bool,
    kindSet :: Set TokenKind
  }
  deriving (Eq, Ord, Show, Generic)

instance NFData TokenSet

-- | Check if a "TokenKind" or EOF is part of the "TokenSet".
member :: Maybe TokenKind -> TokenSet -> Bool
member Nothing set = eofMember set
member (Just k) set = kindMember k set

-- | Check if a "TokenKind" is part of the "TokenSet".
kindMember :: TokenKind -> TokenSet -> Bool
kindMember k set = k `Set.member` kindSet set

null :: TokenSet -> Bool
null set = Set.null (kindSet set) && not (eofMember set)

-- | Insert a "TokenKind" or EOF into a "TokenSet".
insert :: Maybe TokenKind -> TokenSet -> TokenSet
insert Nothing set = insertEof set
insert (Just k) set = insertKind k set

-- | Insert EOF into a "TokenSet".
insertEof :: TokenSet -> TokenSet
insertEof set = set {eofMember = True}

-- | Insert a "TokenKind" into a "TokenSet".
insertKind :: TokenKind -> TokenSet -> TokenSet
insertKind k set = set {kindSet = Set.insert k (kindSet set)}

-- | Get the union of two "TokenSet"s.
union :: TokenSet -> TokenSet -> TokenSet
union left right =
  let unionEofMember = eofMember left || eofMember right
      unionKindSet = kindSet left `Set.union` kindSet right
   in TokenSet {eofMember = unionEofMember, kindSet = unionKindSet}

-- | Get the intersection of two "TokenSet"s.
intersection :: TokenSet -> TokenSet -> TokenSet
intersection left right =
  let intersectionEofMember = eofMember left && eofMember right
      intersectionKindSet = kindSet left `Set.intersection` kindSet right
   in TokenSet {eofMember = intersectionEofMember, kindSet = intersectionKindSet}

-- | The empty "TokenSet".
empty :: TokenSet
empty = TokenSet {eofMember = False, kindSet = Set.empty}

-- | Create a "TokenSet" containing a single "TokenKind" or EOF.
singleton :: Maybe TokenKind -> TokenSet
singleton Nothing = fromEOF
singleton (Just k) = fromKind k

-- | Create a "TokenSet" containing just EOF.
fromEOF :: TokenSet
fromEOF = TokenSet {eofMember = True, kindSet = Set.empty}

-- | Create a "TokenSet" containing a single "TokenKind".
fromKind :: TokenKind -> TokenSet
fromKind k = TokenSet {eofMember = False, kindSet = Set.singleton k}

-- | Create a "TokenSet" from a list of "TokenKind"s.
fromList :: [TokenKind] -> TokenSet
fromList ks = TokenSet {eofMember = False, kindSet = Set.fromList ks}

-- | Get a list of all "TokenKind"s in a "TokenSet".
toList :: TokenSet -> [TokenKind]
toList set = Set.toList (kindSet set)

showTokenSet :: TokenSet -> String
showTokenSet set =
  let kindStrs = map showTokenKind $ toList set
      eofStrs = if eofMember set then ["<eof>"] else []
   in intercalate "," (kindStrs ++ eofStrs)
