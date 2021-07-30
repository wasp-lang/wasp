{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Analyzer.Evaluator.TH (makeDecl, makeEnum) where

import Analyzer.Evaluator.Combinators
import qualified Analyzer.Type as T
import Analyzer.TypeDefinitions.Class
import qualified Data.HashMap.Strict as H
import Language.Haskell.TH
import Util (toLowerFirst)

-- ========================================
-- IsDeclType generation
-- ========================================

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

genDecl :: Con -> Q [DecQ]
genDecl (NormalC nm [(_, typ)]) = genPrimDecl nm typ
genDecl (NormalC nm _) = fail $ "Too many non-record values in makeDecl for " ++ show nm
genDecl (RecC nm recs) = genRecDecl nm $ map (\(recNm, _, typ) -> (recNm, typ)) recs
genDecl _ = fail "makeDecl on non-decl type"

-- For simple decls, i.e. @data Simple = Simple Int@

genPrimDecl :: Name -> Type -> Q [DecQ]
genPrimDecl nm typ = do
  let declTypeNameD = funD 'declTypeName [clause [] (normalB $ litE $ stringL $ toLowerFirst $ nameBase nm) []]
  let declTypeBodyTypeD = funD 'declTypeBodyType [clause [] (normalB $ genTypeE typ) []]
  let declTypeFromASTD =
        funD
          'declTypeFromAST
          [ clause
              []
              ( normalB $
                  varE 'build `appE` infixE (Just $ conE nm) (varE '(<$>)) (Just $ genTransformE typ)
              )
              []
          ]
  pure [declTypeNameD, declTypeBodyTypeD, declTypeFromASTD]

genTypeE :: Type -> ExpQ
genTypeE typ =
  waspKindOfType typ >>= \case
    KString -> conE 'T.StringType
    KInteger -> conE 'T.NumberType
    KDouble -> conE 'T.NumberType
    KBool -> conE 'T.BoolType
    KList elemType -> conE 'T.ListType `appE` genTypeE elemType
    KDecl -> conE 'T.DeclType `appE` (varE 'declTypeName `appTypeE` pure typ)
    KEnum -> conE 'T.EnumType `appE` (varE 'enumTypeName `appTypeE` pure typ)
    KOptional _ -> fail "Maybe only allowed in record fields"

genTransformE :: Type -> ExpQ
genTransformE typ =
  waspKindOfType typ >>= \case
    KString -> varE 'string
    KInteger -> varE 'integer
    KDouble -> varE 'double
    KBool -> varE 'bool
    KList elemType -> varE 'list `appE` genTransformE elemType
    KDecl -> varE 'decl `appTypeE` pure typ
    KEnum -> varE 'enum `appTypeE` pure typ
    KOptional _ -> fail "Maybe only allowed in record fields"

-- For decls with record constructors, i.e. @data Fields = Fields { a :: String, b :: String }

genRecDecl :: Name -> [(Name, Type)] -> Q [DecQ]
genRecDecl nm recs = do
  let declTypeNameD = funD 'declTypeName [clause [] (normalB $ litE $ stringL $ toLowerFirst $ nameBase nm) []]
  -- recs is reversed to make sure the applications for transformDictE are in the right order
  (dictEntryTypesE, transformDictE) <- genRecEntryTypesAndTransform nm $ reverse recs
  let dictTypeE = conE 'T.DictType `appE` (varE 'H.fromList `appE` dictEntryTypesE)
  let transformE = varE 'dict `appE` transformDictE
  let declTypeBodyTypeD = funD 'declTypeBodyType [clause [] (normalB dictTypeE) []]
  let declTypeFromASTD = funD 'declTypeFromAST [clause [] (normalB $ varE 'build `appE` transformE) []]
  pure [declTypeNameD, declTypeBodyTypeD, declTypeFromASTD]

genRecEntryTypesAndTransform :: Name -> [(Name, Type)] -> Q (ExpQ, ExpQ)
genRecEntryTypesAndTransform conNm [] = pure (listE [], varE 'pure `appE` conE conNm)
genRecEntryTypesAndTransform conNm ((recNm, typ) : rest) = do
  (restDictType, restTransform) <- genRecEntryTypesAndTransform conNm rest
  let thisDictType =
        infixE
          (Just $ tupE [litE $ stringL $ nameBase recNm, genFieldTypeE typ])
          (conE '(:))
          (Just restDictType)
  let thisTransform = infixE (Just restTransform) (varE '(<*>)) (Just $ genTransformDictE recNm typ)
  pure (thisDictType, thisTransform)

genFieldTypeE :: Type -> ExpQ
genFieldTypeE typ =
  waspKindOfType typ >>= \case
    KOptional elemType -> conE 'T.DictOptional `appE` genTypeE elemType
    _ -> conE 'T.DictRequired `appE` genTypeE typ

genTransformDictE :: Name -> Type -> ExpQ
genTransformDictE recNm typ =
  waspKindOfType typ >>= \case
    KOptional elemType -> varE 'maybeField `appE` recNmE `appE` genTransformE elemType
    _ -> varE 'field `appE` recNmE `appE` genTransformE typ
  where
    recNmE = litE $ stringL $ nameBase recNm

data WaspKind
  = KString
  | KInteger
  | KDouble
  | KBool
  | KList Type
  | KDecl
  | KEnum
  | KOptional Type

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

-- if typIsPrim
--   then pure Primitive
--   else
--     if typIsDecl
--       then pure Decl
--       else
--         if typIsEnum
--           then pure Enum
--           else case typ of
--             ConT conNm `AppT` typ' | conNm == ''Maybe -> pure $ Optional typ'
--             _ -> pure None

-- ========================================
-- IsEnumType generation
-- ========================================

makeEnum :: Name -> Q [Dec]
makeEnum ty = do
  (TyConI tyCon) <- reify ty
  (tyConName, cons) <- case tyCon of
    (DataD _ nm [] _ cons _) -> pure (nm, cons)
    (NewtypeD _ nm [] _ con _) -> pure (nm, [con])
    _ -> fail "Invalid name for makeEnum"
  let instanceType = conT ''IsEnumType `appT` conT tyConName
  conNames <- enumConNames cons
  sequence [instanceD (return []) instanceType (genEnum tyConName conNames)]

genEnum :: Name -> [Name] -> [DecQ]
genEnum tyConName cons =
  [ genEnumName tyConName,
    genEnumVariants cons,
    genEnumFromVariants cons
  ]

genEnumName :: Name -> DecQ
genEnumName tyConName = do
  let enumTypeNameExp = litE $ stringL $ toLowerFirst $ nameBase tyConName
  let enumTypeNameClause = clause [] (normalB enumTypeNameExp) []
  funD 'enumTypeName [enumTypeNameClause]

genEnumVariants :: [Name] -> DecQ
genEnumVariants conNames = do
  let variantsExp = listE $ map (litE . stringL . nameBase) conNames
  let variantsClause = clause [] (normalB variantsExp) []
  funD 'enumTypeVariants [variantsClause]

genEnumFromVariants :: [Name] -> DecQ
genEnumFromVariants conNames = do
  let clauses = map genClause conNames
  let leftClause = clause [varP $ mkName "x"] (normalB $ conE 'Left `appE` litE (stringL "Invalid variant for enum")) []
  funD 'enumTypeFromVariant (clauses ++ [leftClause])
  where
    genClause :: Name -> ClauseQ
    genClause nm = clause [litP $ stringL (nameBase nm)] (normalB $ conE 'Right `appE` conE nm) []

enumConNames :: [Con] -> Q [Name]
enumConNames = mapM conName
  where
    conName (NormalC nm []) = pure nm
    conName _ = fail "Enum variant should have only one value"
