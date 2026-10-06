{-# LANGUAGE GADTs #-}
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
import Wasp.AppSpec.Core.IsDecl (IsDecl (declName, declTypeName))
import Wasp.Inspectable (Inspectable (..))

-- | A container for any (IsDecl a) type, allowing you to have a heterogenous list of
--   Wasp declarations as [Decl].
--   Declarations make the top level of AppSpec.
data Decl where
  Decl :: (IsDecl a) => a -> Decl

-- | Serializes a declaration into the same JSON envelope that the TS spec
-- produces and 'Wasp.AppSpec.Core.Decl.JSON' parses: {declType, declValue}.
instance ToJSON Decl where
  toJSON (Decl (value :: a)) =
    object
      [ "declType" .= declTypeName @a,
        "declValue" .= value
      ]

instance Inspectable Decl where
  inspect (Decl value) = inspect value

-- | Extracts all declarations of a certain type from a @[Decl]@s
takeDecls :: (IsDecl a) => [Decl] -> [a]
takeDecls = mapMaybe fromDecl

makeDecl :: (IsDecl a) => a -> Decl
makeDecl = Decl

fromDecl :: (IsDecl a) => Decl -> Maybe a
fromDecl (Decl value) = cast value

getDeclName :: Decl -> String
getDeclName (Decl value) = declName value
