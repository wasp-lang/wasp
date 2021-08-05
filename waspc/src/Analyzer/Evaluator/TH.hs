{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

-- This module exports two TH functions, @makeDecl@ and @makeEnum@, which
-- write instances for @IsDeclType@ and @IsEnumType@, respectively. Only correct
-- instances will be generated. If a non-decl or non-enum type name is given to
-- either of these functions, a Haskell type error is raised.
module Analyzer.Evaluator.TH (makeDecl, makeEnum) where

import Analyzer.Evaluator.Combinators
import Analyzer.Evaluator.EvaluationError
import qualified Analyzer.Type as T
import Analyzer.TypeDefinitions.Class
import qualified Data.HashMap.Strict as H
import Language.Haskell.TH
import Util (toLowerFirst)

-- | @makeDecl ''Type@ writes an @IsDeclType@ instance for @Type@. A type
-- error is raised if @Type@ does not fit the criteria described in the
-- definition of @IsDeclType@.
--
-- In addition to satisfying the requirements of "IsDeclType", the generated
-- instance for @Type@ has @declTypeName == "type"@ (the first letter is
-- always changed to lowercase).
--
-- __Example__
--
-- @
-- {-# LANGUAGE TemplateHaskell #-}
-- data Person = Person { name :: String, age :: Int }
-- makeDecl ''Person
-- -- "IsDeclType Person" instance is generated
-- @
makeDecl :: Name -> Q [Dec]
makeDecl typeName = do
  (TyConI tyCon) <- reify typeName
  (tyConName, con) <- case tyCon of
    (DataD _ name [] _ [con] _) -> pure (name, con)
    (NewtypeD _ name [] _ con _) -> pure (name, con)
    _ -> fail "Invalid name for makeDecl"
  let instanceType = conT ''IsDeclType `appT` conT tyConName
  instanceDecs <- genDecl con
  sequence [instanceD (return []) instanceType instanceDecs]

-- | @makeEnum ''Type@ writes an @IsEnumType@ instance for @Type@. A type
-- error is raised if @Type@ does not fit the criteria described in the
-- definition of @IsEnumType@.
--
-- In addition to satisfying the requirements of "IsEnumType", the generaed
-- instance for @Type@ has @enumTypeName == "Type"@ (the name is not modified
-- at all).
--
-- __Example__
--
-- @
-- {-# LANGUAGE TemplateHaskell #-}
-- data Job = Programmer | Manager
-- makeEnum ''Job
-- -- "IsEnumType Job" instance is generated
-- @
makeEnum :: Name -> Q [Dec]
makeEnum typeName = do
  (TyConI tyCon) <- reify typeName
  (tyConName, cons) <- case tyCon of
    (DataD _ name [] _ cons _) -> pure (name, cons)
    (NewtypeD _ name [] _ con _) -> pure (name, [con])
    _ -> fail "Invalid name for makeEnum"
  let instanceType = conT ''IsEnumType `appT` conT tyConName
  conNames <- enumConNames cons
  instanceDecs <- genEnum tyConName conNames
  sequence [instanceD (return []) instanceType instanceDecs]

-- ========================================
-- IsDeclType generation
-- ========================================

-- | Top-level "IsDeclType" instance generator.
genDecl :: Con -> Q [DecQ]
-- The constructor is in the form @data Type = Type x@
genDecl (NormalC name [(_, typ)]) = genPrimDecl name typ
-- The constructor is in the form @data Type = Type x1 x2 ... xn@, which is not valid for a decl
genDecl (NormalC name _) = fail $ "Too many non-record values in makeDecl for " ++ show name
-- The constructor is in the form @data Type = Type { k1 :: f1, ..., kn :: fn }
genDecl (RecC name recs) = genRecDecl name $ map (\(recName, _, typ) -> (recName, typ)) recs
-- The constructor is in an unsupported form
genDecl _ = fail "makeDecl on non-decl type"

-- | Create an "IsDeclType" instance for types in the form @data Type = Type x@
genPrimDecl :: Name -> Type -> Q [DecQ]
genPrimDecl name typ =
  map pure
    <$> [d|
      declTypeName = $(lowerNameStrE name)

      declTypeBodyType = $(genTypeE typ)

      declTypeFromAST = build $ $(conE name) <$> $(genTransformE typ)
      |]

-- | For decls with record constructors, i.e. @data Fields = Fields { a :: String, b :: String }
genRecDecl :: Name -> [(Name, Type)] -> Q [DecQ]
genRecDecl name recs = do
  -- recs is reversed to make sure the applications for transformDictE are in the right order
  (dictEntryTypesE, transformDictE) <- genRecEntryTypesAndTransform name $ reverse recs
  map pure
    <$> [d|
      declTypeName = $(lowerNameStrE name)

      declTypeBodyType = T.DictType $ H.fromList $dictEntryTypesE

      declTypeFromAST = build $ dict $transformDictE
      |]

-- | Write a wasp @Type@ for a Haskell type
genTypeE :: Type -> ExpQ
genTypeE typ =
  waspKindOfType typ >>= \case
    KString -> [|T.StringType|]
    KInteger -> [|T.NumberType|]
    KDouble -> [|T.NumberType|]
    KBool -> [|T.BoolType|]
    KList elemType -> [|T.ListType $(genTypeE elemType)|]
    KDecl -> [|T.DeclType declTypeName @ $(pure typ)|]
    KEnum -> [|T.EnumType enumTypeName @ $(pure typ)|]
    KOptional _ -> fail "Maybe only allowed in record fields"

-- | Write a @Transform@ for a Haskell type
genTransformE :: Type -> ExpQ
genTransformE typ =
  waspKindOfType typ >>= \case
    KString -> [|string|]
    KInteger -> [|integer|]
    KDouble -> [|double|]
    KBool -> [|bool|]
    KList elemType -> [|list $(genTransformE elemType)|]
    KDecl -> [|decl @ $(pure typ)|]
    KEnum -> [|enum @ $(pure typ)|]
    KOptional _ -> fail "Maybe only allowed in record fields"

-- | Write the @DictEntryType@s and @TransformDict@ for the records in a
-- Haskell constructor.
genRecEntryTypesAndTransform :: Name -> [(Name, Type)] -> Q (ExpQ, ExpQ)
genRecEntryTypesAndTransform conName [] = pure (listE [], varE 'pure `appE` conE conName)
genRecEntryTypesAndTransform conName ((recName, typ) : rest) = do
  (restDictType, restTransform) <- genRecEntryTypesAndTransform conName rest
  let thisDictTypeE = [|($(nameStrE recName), $(genFieldTypeE typ)) : $restDictType|]
  let thisTransformE = [|$restTransform <*> $(genTransformDictE recName typ)|]
  pure (thisDictTypeE, thisTransformE)

-- | Write a @DictEntryType@ for a Haskell type.
genFieldTypeE :: Type -> ExpQ
genFieldTypeE typ =
  waspKindOfType typ >>= \case
    KOptional elemType -> [|T.DictOptional $(genTypeE elemType)|]
    _ -> [|T.DictRequired $(genTypeE typ)|]

-- | Write a @TransformDict@ for a Haskell type.
genTransformDictE :: Name -> Type -> ExpQ
genTransformDictE recName typ =
  waspKindOfType typ >>= \case
    KOptional elemType -> [|maybeField $(nameStrE recName) $(genTransformE elemType)|]
    _ -> [|field $(nameStrE recName) $(genTransformE typ)|]

-- | An intermediate mapping between Haskell types and Wasp types, used for
-- generating @Types@, @Transforms@, @DictEntryTypes@, and @TransformDicts@.
data WaspKind
  = KString
  | KInteger
  | KDouble
  | KBool
  | KList Type
  | KDecl
  | KEnum
  | -- | Valid only in a record field, represents @DictOptional@/@Maybe@
    KOptional Type

-- | Find the "WaspKind" of a Haskell type.
waspKindOfType :: Type -> Q WaspKind
waspKindOfType typ = do
  typIsDecl <- isInstance ''IsDeclType [typ]
  typIsEnum <- isInstance ''IsEnumType [typ]
  if typIsDecl
    then pure KDecl
    else
      if typIsEnum
        then pure KEnum
        else case typ of
          ConT name
            | name == ''String -> pure KString
            | name == ''Integer -> pure KInteger
            | name == ''Double -> pure KDouble
            | name == ''Bool -> pure KBool
          ListT `AppT` elemType -> pure (KList elemType)
          ConT name `AppT` elemType | name == ''Maybe -> pure (KOptional elemType)
          _ -> fail $ "No translation to wasp type for type " ++ show typ

-- ========================================
-- IsEnumType generation
-- ========================================

genEnum :: Name -> [Name] -> Q [DecQ]
genEnum tyConName cons =
  map pure . concat
    <$> sequence
      [ [d|
          enumTypeName = $(lowerNameStrE tyConName)

          enumTypeVariants = $(listE $ map nameStrE cons)
          |],
        genEnumFromVariants cons
      ]

genEnumFromVariants :: [Name] -> Q [Dec]
genEnumFromVariants conNames = do
  let clauses = map genClause conNames
  let leftClause = clause [[p|x|]] (normalB [|Left $ EvaluationError $ "Invalid variant " ++ show x ++ " for enum"|]) []
  (: []) <$> funD 'enumTypeFromVariant (clauses ++ [leftClause])
  where
    genClause :: Name -> ClauseQ
    genClause name = clause [litP $ stringL $ nameBase name] (normalB [|Right $(conE name)|]) []

enumConNames :: [Con] -> Q [Name]
enumConNames = mapM conName
  where
    conName (NormalC name []) = pure name
    conName _ = fail "Enum variant should have only one value"

-- ========================================
-- Helper functions
-- ========================================

-- | Get an expression representing the string form of a name, starting with a lowercase letter
lowerNameStrE :: Name -> ExpQ
lowerNameStrE = litE . stringL . toLowerFirst . nameBase

-- | Get an expression representing the string form of a name
nameStrE :: Name -> ExpQ
nameStrE = litE . stringL . nameBase
