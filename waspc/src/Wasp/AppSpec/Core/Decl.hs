{-# LANGUAGE GADTs #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module Wasp.AppSpec.Core.Decl
  ( Decl,
    takeDecls,
    makeDecl,
    fromDecl,
    getDeclName,
    isValidWaspIdentifier,
  )
where

import Data.Aeson (ToJSON (toJSON), object, (.=))
import Data.Char (isAlpha, isAlphaNum)
import Data.Maybe (mapMaybe)
import Data.Typeable (cast)
import Wasp.AppSpec.Core.IsDecl (IsDecl (declTypeName))
import Wasp.Inspectable (Inspectable (..), modifyDatapointList)

-- | A container for any (IsDecl a) type, allowing you to have a heterogenous list of
--   Wasp declarations as [Decl].
--   Declarations make the top level of AppSpec.
data Decl where
  Decl :: (IsDecl a) => String -> a -> Decl

-- | Serializes a declaration into the same JSON envelope that the TS spec
-- produces and 'Wasp.AppSpec.Core.Decl.JSON' parses: {declType, declName, declValue}.
instance ToJSON Decl where
  toJSON (Decl name (value :: a)) =
    object
      [ "declType" .= declTypeName @a,
        "declName" .= name,
        "declValue" .= value
      ]

instance Inspectable Decl where
  inspect (Decl name value) =
    modifyDatapointList (("Name", name) :) <$> inspect value

-- | Extracts all declarations of a certain type from a @[Decl]@s
takeDecls :: (IsDecl a) => [Decl] -> [(String, a)]
takeDecls = mapMaybe fromDecl

makeDecl :: (IsDecl a) => String -> a -> Decl
makeDecl = Decl

fromDecl :: (IsDecl a) => Decl -> Maybe (String, a)
fromDecl (Decl name value) = (name,) <$> cast value

getDeclName :: Decl -> String
getDeclName (Decl name _) = name

-- | Checks if a string is a valid Wasp identifier.
--
-- A valid identifier matches @[_a-zA-Z][_a-zA-Z0-9]*'*@: it
-- starts with a letter or underscore, continues with letters, digits or
-- underscores, may end with any number of apostrophes, and is not one of
-- the reserved keywords.
isValidWaspIdentifier :: String -> Bool
isValidWaspIdentifier str = matchesIdentifierRule str && str `notElem` reservedKeywords
  where
    matchesIdentifierRule [] = False
    matchesIdentifierRule (c : cs) =
      isIdentStart c && all isIdentChar body && all (== '\'') primes
      where
        (body, primes) = span (/= '\'') cs
    isIdentStart ch = isAlpha ch || ch == '_'
    isIdentChar ch = isAlphaNum ch || ch == '_'
    reservedKeywords = ["import", "from", "true", "false"]
