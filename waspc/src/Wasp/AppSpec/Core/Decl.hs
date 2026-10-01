{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}

module Wasp.AppSpec.Core.Decl
  ( Decl,
    takeDecls,
    makeDecl,
    fromDecl,
    getDeclName,
    parseDeclValue,
  )
where

import Data.Aeson (FromJSON (parseJSON), Object, ToJSON (toJSON), Value (Object), object, (.=))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Aeson.Types (Parser)
import Data.Maybe (mapMaybe)
import Data.Typeable (cast)
import Wasp.AppSpec.Core.IsDecl (IsDecl (declName, declTypeName))
import Wasp.Inspectable (Inspectable (..), modifyDatapointList)

-- | A container for any (IsDecl a) type, allowing you to have a heterogenous list of
--   Wasp declarations as [Decl].
--   Declarations make the top level of AppSpec.
data Decl where
  Decl :: (IsDecl a) => a -> Decl

-- | Serializes a declaration into the same JSON envelope that the TS spec
-- produces and 'Wasp.AppSpec.Core.Decl.JSON' parses: {declType, declName, declValue}.
-- The name is carried by @declName@, so we leave it out of @declValue@.
instance ToJSON Decl where
  toJSON (Decl (value :: a)) =
    object
      [ "declType" .= declTypeName @a,
        "declName" .= declName value,
        "declValue" .= case toJSON value of
          Object declValue -> Object $ KeyMap.delete declValueNameKey declValue
          declValue -> declValue
      ]

instance Inspectable Decl where
  inspect (Decl value) =
    modifyDatapointList (("Name", declName value) :) <$> inspect value

-- | Parses the @declValue@ of the JSON envelope into a declaration.
-- The envelope carries the name in @declName@, outside of @declValue@, so we
-- inject it into @declValue@ for the declaration to pick it up.
parseDeclValue :: (FromJSON a) => String -> Object -> Parser a
parseDeclValue name declValue =
  parseJSON $ Object $ KeyMap.insert declValueNameKey (toJSON name) declValue

declValueNameKey :: Key.Key
declValueNameKey = "name"

-- | Extracts all declarations of a certain type from a @[Decl]@s
takeDecls :: (IsDecl a) => [Decl] -> [a]
takeDecls = mapMaybe fromDecl

makeDecl :: (IsDecl a) => a -> Decl
makeDecl = Decl

fromDecl :: (IsDecl a) => Decl -> Maybe a
fromDecl (Decl value) = cast value

getDeclName :: Decl -> String
getDeclName (Decl value) = declName value
