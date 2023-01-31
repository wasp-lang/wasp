{-# LANGUAGE TemplateHaskell #-}

module Wasp.Analyzer.TypeDefinitions.TH.Enum
  ( makeEnumType,
  )
where

import Language.Haskell.TH
import Wasp.Analyzer.TypeDefinitions (EnumType (..), IsEnumType (..))
import qualified Wasp.Analyzer.TypeDefinitions.TH.Common as THC

-- | @makeEnumType ''Type@ writes an @IsEnumType@ instance for @Type@. A type
-- error is raised if @Type@ does not fit the criteria described below.
--
-- Requirements on @Type@ for this to work:
--   - The type must be an ADT with at least one constructor.
--   - Each constructor of the type must have 0 fields.
--
-- Properties that hold for @Type@ and its generated instance of @IsEnumType@:
--  - If and only if there is a string @x@ in @etVariants (enumType \@Type)@, then @Type@ has
--    a constructor called @x@.
--  - @etName (enumType \@Type) == "Type"@ -> enum type has same name as @Type@.
--
-- __Example__
--
-- @
-- >>> data AuthMethod = OAuth2 | UsernameAndPassword deriving Generic
-- >>> makeEnumType ''AuthMethod  -- "IsEnumType AuthMethod" instance is generated.
-- >>> etName $ enumType @AuthMethod
-- "authMethod"
-- >>> etVariants $ enumType @AuthMethod
-- ["OAuth2", "UsernameAndPassword"]
-- @
makeEnumType :: Name -> Q [Dec]
makeEnumType typeName = do
  (TyConI tyCon) <- reify typeName
  dataConstructors <- case tyCon of
    (DataD _ _ [] _ dataConstructors _) -> pure dataConstructors
    DataD {} -> fail "makeEnumType expects a type declared with `data` to have no type variables."
    (NewtypeD _ _ [] _ dataConstructor _) -> pure [dataConstructor]
    NewtypeD {} -> fail "makeEnumType expects a type declared with `newtype` to have no type variables."
    _ -> fail "makeEnumType expects the given name to be for a type declared with `data` or `newtype`."
  dataConstructorNames <- namesOfEnumDataConstructors dataConstructors
  let instanceDeclaration = instanceD instanceContext instanceType =<< instanceDefinition
      instanceContext = pure []
      instanceType = [t|IsEnumType $(conT typeName)|]
      instanceDefinition = makeIsEnumTypeDefinition typeName dataConstructorNames
  sequence [instanceDeclaration]

namesOfEnumDataConstructors :: [Con] -> Q [Name]
namesOfEnumDataConstructors = mapM conName
  where
    conName (NormalC name []) = pure name
    conName _ = fail "Enum variant should have only one value"

makeIsEnumTypeDefinition :: Name -> [Name] -> Q [DecQ]
makeIsEnumTypeDefinition typeName dataConstructorNames =
  pure
    [ genEnumType typeName dataConstructorNames,
      genEnumEvaluate dataConstructorNames
    ]

genEnumType :: Name -> [Name] -> DecQ
genEnumType typeName dataConstructorNames =
  THC.genFunc
    'enumType
    [|
      EnumType
        { etName = $(THC.nameToLowerFirstStringLiteralExpr typeName),
          etVariants = $(listE $ map THC.nameToStringLiteralExpr dataConstructorNames)
        }
      |]

genEnumEvaluate :: [Name] -> DecQ
genEnumEvaluate dataConstructorNames = do
  let clauses = map genClause dataConstructorNames
  let invalidVariantClause = clause [[p|_|]] (normalB [|Nothing|]) []
  funD 'enumEvaluate (clauses ++ [invalidVariantClause])
  where
    genClause :: Name -> ClauseQ
    genClause name = clause [litP $ stringL $ nameBase name] (normalB [|Just $(conE name)|]) []
