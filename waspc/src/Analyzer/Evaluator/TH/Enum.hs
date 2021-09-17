{-# LANGUAGE TemplateHaskell #-}

module Analyzer.Evaluator.TH.Enum
  ( makeEnumType,
  )
where

import Analyzer.Evaluator.EvaluationError
import Analyzer.Evaluator.TH.Common
import Analyzer.TypeDefinitions (EnumType (..), IsEnumType (..))
import Language.Haskell.TH

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
-- >>> data AuthMethod = OAuth2 | EmailAndPassword deriving Generic
-- >>> makeEnumType ''AuthMethod  -- "IsEnumType AuthMethod" instance is generated.
-- >>> etName $ enumType @AuthMethod
-- "authMethod"
-- >>> etVariants $ enumType @AuthMethod
-- ["OAuth2", "EmailAndPassword"]
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

makeIsEnumTypeDefinition :: Name -> [Name] -> Q [DecQ]
makeIsEnumTypeDefinition typeName dataConstructorNames =
  pure
    [ genFunc
        'enumType
        [|
          EnumType
            { etName = $(nameToLowerFirstStringLiteralExpr typeName),
              etVariants = $(listE $ map nameToStringLiteralExpr dataConstructorNames)
            }
          |],
      genEnumFromVariants
        typeName
        dataConstructorNames
    ]

genEnumFromVariants :: Name -> [Name] -> DecQ
genEnumFromVariants typeName dataConstructorNames = do
  let clauses = map genClause dataConstructorNames
  let leftClause = clause [[p|x|]] (normalB [|Left $ InvalidEnumVariant $(nameToStringLiteralExpr typeName) (show x)|]) []
  funD 'enumTypeFromVariant (clauses ++ [leftClause])
  where
    genClause :: Name -> ClauseQ
    genClause name = clause [litP $ stringL $ nameBase name] (normalB [|Right $(conE name)|]) []

namesOfEnumDataConstructors :: [Con] -> Q [Name]
namesOfEnumDataConstructors = mapM conName
  where
    conName (NormalC name []) = pure name
    conName _ = fail "Enum variant should have only one value"
