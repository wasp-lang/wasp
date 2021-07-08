{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Analyzer.TypeDefinitions
  ( TypeDefinitions,
    empty,
    getDeclType,
    getEnumType,
    addDeclType,
    addEnumType,
    DeclType (..),
    EnumType (..),
    IsDeclType,
    IsEnumType,
  )
where

import Analyzer.Type (Type)
import qualified Data.HashMap.Strict as M

data EnumType = EnumType {etName :: String, etVariants :: [String]}

data DeclType = DeclType {dtName :: String, dtBodyType :: Type}

-- | Defines the declaration types and enum type that Analyzer is to take into
--   consideration
--
--   The are used during type-checking and for constructing instances of
--   Haskell types that correspond to these declaration and enums.
data TypeDefinitions = TypeDefinitions
  { declTypes :: M.HashMap String DeclType,
    enumTypes :: M.HashMap String EnumType
  }

-- | A library with no types
empty :: TypeDefinitions
empty = TypeDefinitions {declTypes = M.empty, enumTypes = M.empty}

getDeclType :: String -> TypeDefinitions -> Maybe DeclType
getDeclType name (TypeDefinitions dts _) = M.lookup name dts

getEnumType :: String -> TypeDefinitions -> Maybe EnumType
getEnumType name (TypeDefinitions _ ets) = M.lookup name ets

-- | Add a declaration type to type definitions. Requires the type to be in the form
--   of a Wasp decl. See "IsDecl" for requirements.
--
--  Example:
--
--  @
--  {-# LANGUAGE TypeApplications, DeriveGeneric #-}
--  data App = App { title :: String } deriving Generic
--  let exampleDefinitions = addDeclType @App empty
--  @
addDeclType :: forall typ. (IsDeclType typ) => TypeDefinitions -> TypeDefinitions
addDeclType lib =
  let decl = DeclType {dtName = _declTypeName @typ, dtBodyType = _declTypeBodyType @typ}
   in lib {declTypes = M.insert (_declTypeName @typ) decl $ declTypes lib}

-- | Add an enum type to type definitions. Requires the type to be in the form
--   of a Wasp enum. See "IsEnum" for requirements.
--
--   Example:
--
--   @
--   {-# LANGUAGE TypeApplications, DeriveGeneric #-}
--   data ParamType = StringParam | NumberParam deriving Generic
--   let exampleDefinitions = addEnumType @ParamType empty
--   @
addEnumType :: forall typ. (IsEnumType typ) => TypeDefinitions -> TypeDefinitions
addEnumType lib =
  let enum = EnumType {etName = _enumTypeName @typ, etVariants = _enumTypeVariants @typ}
   in lib {enumTypes = M.insert (_enumTypeName @typ) enum $ enumTypes lib}

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
--
--   Examples:
--
--   Using record fields for dictionary types:
--
--   @
--   >>> data User = User { name :: String, email :: Maybe String } deriving Generic
--   >>> _declName @User
--   "user"
--   >>> _declType @User
--   DictType [DictEntry "name" StringType, DictOptionalEntry "email" StringLiteral]
--   @
--
--   No records for a type that doesn't use a dictionary:
--
--   @
--   >>> data Admins = Admins [User] deriving Generic
--   >>> _declType @Admins
--   ListType (DeclType "User")
--   @
class IsDeclType a where
  _declTypeName :: String
  _declTypeBodyType :: Type

-- | Encodes the requirements of a Wasp enum.
--
--   For an instance to be created for a type with a `Generic` instance,
--   - The type must be an ADT with at least one constructor
--   - Each constructor of the type must have 0 fields
--
--   Some properties are required of `_enumVariants` and `a`:
--   - If and only if there is a string `x` in `_enumVariants`, then `a` has
--     a constructor called `x`.
--
--    Examples:
--
--   An allowed enum type:
--
--   @
--   >>> data AuthMethod = OAuth2 | EmailAndPassword deriving Generic
--   >>> _enumTypeName @AuthMethod
--   "authMethod"
--   >>> _enumTypeVariants @AuthMethod
--   ["OAuth2", "EmailAndPassword"]
--   @
class IsEnumType a where
  _enumTypeName :: String
  _enumTypeVariants :: [String]
