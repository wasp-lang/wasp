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
makeDecl ty = do
  (TyConI tyCon) <- reify ty
  (tyConName, con) <- case tyCon of
    (DataD _ nm [] _ [con] _) -> pure (nm, con)
    (NewtypeD _ nm [] _ con _) -> pure (nm, con)
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
makeEnum ty = do
  (TyConI tyCon) <- reify ty
  (tyConName, cons) <- case tyCon of
    (DataD _ nm [] _ cons _) -> pure (nm, cons)
    (NewtypeD _ nm [] _ con _) -> pure (nm, [con])
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
genDecl (NormalC nm [(_, typ)]) = genPrimDecl nm typ
-- The constructor is in the form @data Type = Type x1 x2 ... xn@, which is not valid for a decl
genDecl (NormalC nm _) = fail $ "Too many non-record values in makeDecl for " ++ show nm
-- The constructor is in the form @data Type = Type { k1 :: f1, ..., kn :: fn }
genDecl (RecC nm recs) = genRecDecl nm $ map (\(recNm, _, typ) -> (recNm, typ)) recs
-- The constructor is in an unsupported form
genDecl _ = fail "makeDecl on non-decl type"

-- | Create an "IsDeclType" instance for types in the form @data Type = Type x@
genPrimDecl :: Name -> Type -> Q [DecQ]
genPrimDecl nm typ =
  map pure
    <$> [d|
      declTypeName = $(lowerNameStrE nm)

      declTypeBodyType = $(genTypeE typ)

      declTypeFromAST = build $ $(conE nm) <$> $(genTransformE typ)
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

-- | For decls with record constructors, i.e. @data Fields = Fields { a :: String, b :: String }
genRecDecl :: Name -> [(Name, Type)] -> Q [DecQ]
genRecDecl nm recs = do
  -- recs is reversed to make sure the applications for transformDictE are in the right order
  (dictEntryTypesE, transformDictE) <- genRecEntryTypesAndTransform nm $ reverse recs
  map pure
    <$> [d|
      declTypeName = $(lowerNameStrE nm)

      declTypeBodyType = T.DictType $ H.fromList $dictEntryTypesE

      declTypeFromAST = build $ dict $transformDictE
      |]

-- | Write the @DictEntryType@s and @TransformDict@ for the records in a
-- Haskell constructor.
genRecEntryTypesAndTransform :: Name -> [(Name, Type)] -> Q (ExpQ, ExpQ)
genRecEntryTypesAndTransform conNm [] = pure (listE [], varE 'pure `appE` conE conNm)
genRecEntryTypesAndTransform conNm ((recNm, typ) : rest) = do
  (restDictType, restTransform) <- genRecEntryTypesAndTransform conNm rest
  let thisDictTypeE = [|($(nameStrE recNm), $(genFieldTypeE typ)) : $restDictType|]
  let thisTransformE = [|$restTransform <*> $(genTransformDictE recNm typ)|]
  pure (thisDictTypeE, thisTransformE)

-- | Write a @DictEntryType@ for a Haskell type.
genFieldTypeE :: Type -> ExpQ
genFieldTypeE typ =
  waspKindOfType typ >>= \case
    KOptional elemType -> [|T.DictOptional $(genTypeE elemType)|]
    _ -> [|T.DictRequired $(genTypeE typ)|]

-- | Write a @TransformDict@ for a Haskell type.
genTransformDictE :: Name -> Type -> ExpQ
genTransformDictE recNm typ =
  waspKindOfType typ >>= \case
    KOptional elemType -> [|maybeField $(nameStrE recNm) $(genTransformE elemType)|]
    _ -> [|field $(nameStrE recNm) $(genTransformE typ)|]

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
          ConT nm
            | nm == ''String -> pure KString
            | nm == ''Integer -> pure KInteger
            | nm == ''Double -> pure KDouble
            | nm == ''Bool -> pure KBool
          ListT `AppT` elemType -> pure (KList elemType)
          ConT nm `AppT` elemType | nm == ''Maybe -> pure (KOptional elemType)
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
    genClause nm = clause [litP $ stringL $ nameBase nm] (normalB [|Right $(conE nm)|]) []

enumConNames :: [Con] -> Q [Name]
enumConNames = mapM conName
  where
    conName (NormalC nm []) = pure nm
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
