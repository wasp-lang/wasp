{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module Wasp.AppSpec.Core.Decl
  ( Decl,
    takeDecls,
    makeDecl,
    fromDecl,
  )
where

import Data.Aeson (FromJSON (parseJSON), withObject, (.:))
import Data.Maybe (mapMaybe)
import Data.Typeable (cast)
import Wasp.AppSpec.Core.IsDecl (IsDecl (declTypeName))
import Wasp.AppSpec.Page (Page)
import Wasp.AppSpec.Route (Route)

-- | A container for any (IsDecl a) type, allowing you to have a heterogenous list of
--   Wasp declarations as [Decl].
--   Declarations make the top level of AppSpec.
data Decl where
  Decl :: (IsDecl a) => String -> a -> Decl

-- | Extracts all declarations of a certain type from a @[Decl]@s
takeDecls :: (IsDecl a) => [Decl] -> [(String, a)]
takeDecls = mapMaybe fromDecl

makeDecl :: (IsDecl a) => String -> a -> Decl
makeDecl = Decl

fromDecl :: (IsDecl a) => Decl -> Maybe (String, a)
fromDecl (Decl name value) = (name,) <$> cast value

instance FromJSON Decl where
  parseJSON = withObject "Decl" $ \o -> do
    declType <- o .: "declType"
    declName <- o .: "declName"
    -- TODO: Add the rest of the decls.
    -- Even better would be to generate this code with Template Haskell:
    -- Find all instances of IsDecl and generate this code for them.
    -- I might need, for this to work, centralize IsDecl instance definitions
    -- in this file instead of them being scattered through AppSpec. Look into
    -- that in any case.
    case declType of
      t | t == declTypeName @Page -> do
        declValue <- o .: "declValue"
        pure $ makeDecl @Page declName declValue
      t | t == declTypeName @Route -> do
        declValue <- o .: "declValue"
        pure $ makeDecl @Route declName declValue
      _unknownDeclType -> fail $ "Unknown declType " <> declType
