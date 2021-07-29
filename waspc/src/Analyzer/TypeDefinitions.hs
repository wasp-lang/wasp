{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Analyzer.TypeDefinitions
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

import Analyzer.Evaluator.Decl (Decl (Decl))
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
addDeclType lib =
  let decl =
        DeclType
          { dtName = declTypeName @typ,
            dtBodyType = declTypeBodyType @typ,
            dtDeclFromAST = \typeDefs bindings name value -> Decl name <$> declTypeFromAST @typ typeDefs bindings value
          }
   in lib {declTypes = M.insert (declTypeName @typ) decl $ declTypes lib}

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
  let enum = EnumType {etName = enumTypeName @typ, etVariants = enumTypeVariants @typ}
   in lib {enumTypes = M.insert (enumTypeName @typ) enum $ enumTypes lib}
