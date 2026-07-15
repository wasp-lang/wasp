{-# LANGUAGE GADTs #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module Wasp.AppSpec.Core.Decl
  ( Decl,
    takeDecls,
    makeDecl,
    fromDecl,
    getDeclName,
  )
where

import Data.Aeson (ToJSON (toJSON), object, (.=))
import Data.Maybe (mapMaybe)
import Data.Typeable (cast)
import Wasp.AppSpec.Core.Inspectable (Inspectable (..), mapDatapointList)
import Wasp.AppSpec.Core.IsDecl (IsDecl (declTypeName))

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

-- | Extracts all declarations of a certain type from a @[Decl]@s
takeDecls :: (IsDecl a) => [Decl] -> [(String, a)]
takeDecls = mapMaybe fromDecl

makeDecl :: (IsDecl a) => String -> a -> Decl
makeDecl = Decl

fromDecl :: (IsDecl a) => Decl -> Maybe (String, a)
fromDecl (Decl name value) = (name,) <$> cast value

getDeclName :: Decl -> String
getDeclName (Decl name _) = name

instance Inspectable Decl where
  inspect (Decl name value) =
    mapDatapointList (("Name", name) :) <$> inspect value
