{-# LANGUAGE AllowAmbiguousTypes #-}

module Analyzer.TypeDefinitions.Class
  ( IsDeclType (..),
    IsEnumType (..),
  )
where

import Analyzer.Evaluator.EvaluationError (EvaluationError)
import Analyzer.TypeDefinitions.Type
import Data.Typeable (Typeable)

-- | Marks haskell type as a representation of a specific Wasp declaration type.
--   Instead of defining a Wasp declaration type manually by constructing it with constructors from
--   Analyzer.Type, this allows us to specify it as a haskell type which knows how to translate itself into Analyzer.Type representation and back.
--   If this haskell type satisfies certain requirements, the knowledge to translate itself back and forth into Analyzer.Type representation can be automatically derived from its shape.
--
--  Requirements on type:
--   - The type must be an instance of `Generic`.
--   - The type must be an ADT with one constructor.
--   - The type must have just one field OR use record syntax (in which case it can have multiple fields).
--
--   Some assumptions are required of `declType` and `a`:
--   - If and only if `declType` is a `Dict`, then
--     - `a` uses record syntax.
--     - If and only if there is a key `x` in `declType`, then `a` has a
--       record `x` with the same type.
--     - If a key `x` is optional, then the record `x` in `a` is a `Maybe`
--   - Otherwise, `a` has one field and `declType` maps to the type of that
--     field.
--
--   Examples:
--
--   Using record fields for dictionary types:
--
--   @
--   >>> data User = User { name :: String, email :: Maybe String } deriving Generic
--   >>> dtName $ declType @User
--   "user"
--   >>> dtBodyType $ declType @User
--   DictType [DictEntry "name" StringType, DictOptionalEntry "email" StringLiteral]
--   @
--
--   No records for a type that doesn't use a dictionary:
--
--   @
--   >>> data Admins = Admins [User] deriving Generic
--   >>> dtBodyType $ declType @Admins
--   ListType (DeclType "User")
--   @
class Typeable a => IsDeclType a where
  declType :: DeclType

-- | Marks Haskell type as a representation of a specific Wasp enum type.
--   Check "IsDeclType" above for more details.
--
-- Requirements on type:
--   - The type must be an instance of 'Generic'.
--   - The type must be an ADT with at least one constructor.
--   - Each constructor of the type must have 0 fields.
--
--   Some properties are required of `etVariants` (in generated `EnumType`) and `a`:
--   - If and only if there is a string `x` in `etVariants`, then `a` has
--     a constructor called `x`.
--
--   Examples:
--
--   An allowed enum type:
--
--   @
--   >>> data AuthMethod = OAuth2 | EmailAndPassword deriving Generic
--   >>> etName $ enumType @AuthMethod
--   "authMethod"
--   >>> etVariants $ enumType @AuthMethod
--   ["OAuth2", "EmailAndPassword"]
--   @
class Typeable a => IsEnumType a where
  enumType :: EnumType

  -- | Converts a string to a Haskell value of this type.
  --
  -- @mapM_ enumTypeFromVariant (etVariants enumType) == Right ()@ should be true
  -- for all instances of "IsEnumType".
  --
  -- __Examples__
  --
  -- >>> data Example = Foo | Bar
  -- >>> instance IsEnumType Example where {- omitted -}
  -- >>> enumTypeFromVariant "Foo"
  -- Foo
  enumTypeFromVariant :: String -> Either EvaluationError a
