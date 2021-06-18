{-# LANGUAGE AllowAmbiguousTypes, TypeApplications, ScopedTypeVariables #-}

module Analyzer.Lib
  ( Lib (..)
  , empty
  , addEnumType
  , addDeclType
  , EnumType (..)
  , DeclType (..)
  , IsEnum
  , IsDecl
  ) where

import Analyzer.Type (Type)
import qualified Data.HashMap.Strict as M

newtype EnumType = EnumType { variants :: [String] }

newtype DeclType = DeclType { declType :: Type }

-- | Contains information for type-checking declarations and enums and for
--   constructing instances of Haskell types corresponding to the declarations
--   and enums.
data Lib = Lib { decls :: M.HashMap String DeclType
               , enums :: M.HashMap String EnumType
               }

-- | A library with no types
empty :: Lib
empty = Lib { decls = M.empty, enums = M.empty }

-- | Add a declaration type to a library. Requires the type to be in the form
--   of a Wasp decl. See "IsDecl" for requirements.
--
--  Example:
--
--  @
--  {-# LANGUAGE TypeApplications, DeriveGeneric #-}
--  data App = App { title :: String } deriving Generic
--  let exampleLib = addDeclType @App empty
--  @
addDeclType :: forall typ. (IsDecl typ) => Lib -> Lib
addDeclType lib = let decl = DeclType { declType = _declType @typ }
                  in  lib { decls = M.insert (_declName @typ) decl $ decls lib }

-- | Add an enum type to a library. Requires the type to be in the form
--   of a Wasp enum. See "IsEnum" for requirements.
--
--   Example:
--
--   @
--   {-# LANGUAGE TypeApplications, DeriveGeneric #-}
--   data ParamType = StringParam | NumberParam deriving Generic
--   let exampleLib = addEnumType @ParamType empty
--   @
addEnumType :: forall typ. (IsEnum typ) => Lib -> Lib
addEnumType lib = let enum = EnumType { variants = _enumVariants @typ }
                  in  lib { enums = M.insert (_enumName @typ) enum $ enums lib }

-- | Encodes the requirements of a Wasp decl.
--
--   For an instance to be created for a type with a `Generic` instance,
--   - The type must be an ADT with one constructor
--   - The type must have one field OR use record syntax
--
--   Some assumptions are required of `_declType` and `a`:
--   - If `_declType` is a `Dict`, then
--     - `a` uses record syntax.
--     - If and only if there is a key `x` in `_declType`, then `a` has a
--       record `x` with the same type.
--     - If a key `x` is optional, then the record `x` in `a` is a `Maybe`
--   - Otherwise, `a` has one field and `_declType` maps to the type of that
--     field.
class IsDecl a where
  _declName :: String
  _declType :: Type

-- | Encodes the requirements of a Wasp enum.
--
--   For an instance to be created for a type with a `Generic` instance,
--   - The type must be an ADT with at least one constructor
--   - Each constructor of the type must have 0 fields
--
--   Some properties are required of `_enumVariants` and `a`:
--   - If and only if there is a string `x` in `_enumVariants`, then `a` has
--     a constructor called `x`.
class IsEnum a where
  _enumName :: String
  _enumVariants :: [String]
