{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Analyzer.TypeDefinitions
-- TODO: Don't export internals of TypeDefinitions! So, we should have TypeDefinitions here, not TypeDefinitions(..).
--   We can put internals into Internal if we need them for testing, or we can use the functions exposed here in tests also.
  ( TypeDefinitions (..),
    empty,
    getDeclTypes,
    getEnumTypes,
    getDeclType,
    getEnumType,
    addDeclType,
    addEnumType,
    DeclType (..),
    EnumType (..),
    IsDeclType (..),
    IsEnumType (..),
  )
where

import Analyzer.TypeDefinitions.Class
import Analyzer.TypeDefinitions.Type
import qualified Data.HashMap.Strict as M

empty :: TypeDefinitions
empty = TypeDefinitions {declTypes = M.empty, enumTypes = M.empty}

getDeclTypes :: TypeDefinitions -> [DeclType]
getDeclTypes = M.elems . declTypes

getEnumTypes :: TypeDefinitions -> [EnumType]
getEnumTypes = M.elems . enumTypes

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
addDeclType typeDefs =
  let declType' = declType @typ
   in typeDefs {declTypes = M.insert (dtName declType') declType' $ declTypes typeDefs}

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
  let enumType' = enumType @typ
   in lib {enumTypes = M.insert (etName enumType') enumType' $ enumTypes lib}
