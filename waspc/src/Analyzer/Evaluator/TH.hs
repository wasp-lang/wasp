{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

-- This module exports two TH functions, @makeDeclType@ and @makeEnumType@, which
-- write instances for @IsDeclType@ and @IsEnumType@, respectively. Only correct
-- instances will be generated. If a non-decl or non-enum type name is given to
-- either of these functions, a Haskell type error is raised.
module Analyzer.Evaluator.TH
  ( makeDeclType,
    makeEnumType,
  )
where

-- TODO:
-- Split into a module for Decls and a module for Enums

import Analyzer.Evaluator.Combinators
import Analyzer.Evaluator.Decl.Operations (makeDecl)
import Analyzer.Evaluator.EvaluationError
import qualified Analyzer.Evaluator.Types as E
import qualified Analyzer.Type as T
import Analyzer.TypeDefinitions.Class
import Analyzer.TypeDefinitions.Type as TD
import qualified Data.HashMap.Strict as H
import Language.Haskell.TH
import Util (toLowerFirst)

-- | @makeDeclType ''Type@ writes an @IsDeclType@ instance for @Type@. A type
-- error is raised if @Type@ does not fit the criteria described in the
-- definition of @IsDeclType@.
--
-- In addition to satisfying the requirements of "IsDeclType", the generated
-- instance for @Type@ has @dtName declType == "type"@ (the first letter is
-- always changed to lowercase).
--
-- __Example__
--
-- @
-- {-# LANGUAGE TemplateHaskell #-}
-- data Person = Person { name :: String, age :: Int }
-- makeDeclType ''Person
-- -- "IsDeclType Person" instance is generated
-- @
makeDeclType :: Name -> Q [Dec]
makeDeclType typeName = do
  (TyConI typeDeclaration) <- reify typeName
  dataConstructor <- case typeDeclaration of
    (DataD _ _ [] _ [dataConstructor] _) -> pure dataConstructor
    DataD {} -> fail "makeDeclType expects a type declared with `data` to have exactly one constructor"
    (NewtypeD _ _ [] _ dataConstructor _) -> pure dataConstructor
    _ -> fail "makeDeclType expects the given name to be for a type declared with `data` or `newtype`"
  let instanceDeclaration = instanceD instanceContext instanceType =<< instanceDefinition
      instanceContext = pure []
      instanceType = [t|IsDeclType $(conT typeName)|]
      instanceDefinition = makeIsDeclTypeInstanceDefinition dataConstructor
  sequence [instanceDeclaration]

-- | @makeEnumType ''Type@ writes an @IsEnumType@ instance for @Type@. A type
-- error is raised if @Type@ does not fit the criteria described in the
-- definition of @IsEnumType@.
--
-- In addition to satisfying the requirements of "IsEnumType", the generated
-- instance returns EnumType that has the same name as @Type@ (the name is not modified at all).
--
-- __Example__
--
-- @
-- {-# LANGUAGE TemplateHaskell #-}
-- data Job = Programmer | Manager
-- makeEnumType ''Job
-- -- "IsEnumType Job" instance is generated
-- @
makeEnumType :: Name -> Q [Dec]
makeEnumType typeName = do
  (TyConI tyCon) <- reify typeName
  dataConstructors <- case tyCon of
    (DataD _ _ [] _ dataConstructors _) -> pure dataConstructors
    (NewtypeD _ _ [] _ dataConstructor _) -> pure [dataConstructor]
    _ -> fail "makeEnumType expects the given name to be for a type declared with `data` or `newtype`"
  dataConstructorNames <- namesOfEnumDataConstructors dataConstructors
  let instanceDeclaration = instanceD instanceContext instanceType =<< instanceDefinition
      instanceContext = pure []
      instanceType = [t|IsEnumType $(conT typeName)|]
      instanceDefinition = makeIsEnumTypeDefinition typeName dataConstructorNames
  sequence [instanceDeclaration]

-- ========================================
-- IsDeclType generation
-- ========================================

-- | Top-level "IsDeclType" instance generator.
makeIsDeclTypeInstanceDefinition :: Con -> Q [DecQ]
-- The constructor is in the form @data Type = Type x@
makeIsDeclTypeInstanceDefinition _dataConstructor@(NormalC name [(_, typ)]) =
  genIsDeclTypeInstanceDefinitionFromNormalDataConstructor name typ
-- The constructor is in the form @data Type = Type x1 x2 ... xn@, which is not valid for a decl
makeIsDeclTypeInstanceDefinition _dataConstructor@(NormalC name values) =
  fail $
    "makeDeclType expects given type " ++ show name
      ++ " to be a record or to have one data constructor with exactly 1 value, "
      ++ "but instead it was given a data constructor with "
      ++ show (length values)
      ++ "values."
-- The constructor is in the form @data Type = Type { k1 :: f1, ..., kn :: fn }
makeIsDeclTypeInstanceDefinition _dataConstructor@(RecC name records) =
  genIsDeclTypeInstanceDefinitionFromRecordDataConstructor name $ map (\(fieldName, _, typ) -> (fieldName, typ)) records
-- The constructor is in an unsupported form
makeIsDeclTypeInstanceDefinition _ = fail "makeDeclType expects given type to have a normal or record constructor"

-- | Create an "IsDeclType" instance for types that have a single data constructor which has a single value, e.g. @data Type = Type x@.
genIsDeclTypeInstanceDefinitionFromNormalDataConstructor :: Name -> Type -> Q [DecQ]
genIsDeclTypeInstanceDefinitionFromNormalDataConstructor dataConstructorName dataConstructorType = do
  let evaluateE = [|runEvaluator $ $(conE dataConstructorName) <$> $(genEvaluatorExprForHaskellType dataConstructorType)|]
  let bodyTypeE = genWaspTypeFromHaskellType dataConstructorType
  pure [genDeclTypeFuncOfIsDeclTypeInstance dataConstructorName bodyTypeE evaluateE]

-- | For decls with record constructors, i.e. @data Fields = Fields { a :: String, b :: String }
genIsDeclTypeInstanceDefinitionFromRecordDataConstructor :: Name -> [(Name, Type)] -> Q [DecQ]
genIsDeclTypeInstanceDefinitionFromRecordDataConstructor dataConstructorName recs = do
  -- recs is reversed to make sure the applications for dictEvaluatorE are in the right order
  (dictEntryTypesE, dictEvaluatorE) <- genDictEntryTypesAndEvaluatorForRecord dataConstructorName $ reverse recs
  let evaluateE = [|runEvaluator $ dict $dictEvaluatorE|]
  let bodyTypeE = [|T.DictType $ H.fromList $dictEntryTypesE|]
  pure [genDeclTypeFuncOfIsDeclTypeInstance dataConstructorName bodyTypeE evaluateE]

-- | Generates 'declType' function for a definition of IsDeclType instance.
-- A helper function for 'genPrimDecl' and 'genRecDecl'.
genDeclTypeFuncOfIsDeclTypeInstance :: Name -> ExpQ -> ExpQ -> DecQ
genDeclTypeFuncOfIsDeclTypeInstance dataConstructorName bodyTypeE evaluateE =
  func
    'declType
    [|
      TD.DeclType
        { TD.dtName = $(nameToLowerFirstStringLiteralExpr dataConstructorName),
          TD.dtBodyType = $bodyTypeE,
          TD.dtEvaluate = \typeDefs bindings declName declBodyExpr ->
            makeDecl declName <$> $evaluateE typeDefs bindings declBodyExpr
        }
      |]

-- | Generates an expression that constructs a wasp @Type@ that corresponds to a given Haskell type.
-- Haskell type -> Wasp type.
genWaspTypeFromHaskellType :: Type -> ExpQ
genWaspTypeFromHaskellType typ =
  waspKindOfType typ >>= \case
    KString -> [|T.StringType|]
    KInteger -> [|T.NumberType|]
    KDouble -> [|T.NumberType|]
    KBool -> [|T.BoolType|]
    KList elemType -> [|T.ListType $(genWaspTypeFromHaskellType elemType)|]
    KImport -> [|T.ExtImportType|]
    KJSON -> [|T.QuoterType "json"|]
    KPSL -> [|T.QuoterType "psl"|]
    KDecl -> [|T.DeclType $ dtName $ declType @ $(pure typ)|]
    KEnum -> [|T.EnumType $ etName $ enumType @ $(pure typ)|]
    KOptional _ -> fail "Maybe is only allowed in record fields"

-- | Generates and expression that is @Evaluator@ that evaluates to a given Haskell type.
genEvaluatorExprForHaskellType :: Type -> ExpQ
genEvaluatorExprForHaskellType typ =
  waspKindOfType typ >>= \case
    KString -> [|string|]
    KInteger -> [|integer|]
    KDouble -> [|double|]
    KBool -> [|bool|]
    KList elemType -> [|list $(genEvaluatorExprForHaskellType elemType)|]
    KImport -> [|extImport|]
    KJSON -> [|json|]
    KPSL -> [|psl|]
    KDecl -> [|decl @ $(pure typ)|]
    KEnum -> [|enum @ $(pure typ)|]
    KOptional _ -> fail "Maybe is only allowed in record fields"

-- | Write the @DictEntryType@s and @DictEvaluator@ for the Haskell record with given data constructor name and fields.
genDictEntryTypesAndEvaluatorForRecord :: Name -> [(Name, Type)] -> Q (ExpQ, ExpQ)
genDictEntryTypesAndEvaluatorForRecord dataConstructorName [] = pure (listE [], varE 'pure `appE` conE dataConstructorName)
genDictEntryTypesAndEvaluatorForRecord dataConstructorName ((fieldName, fieldType) : restOfFields) = do
  (restDictType, restEvaluator) <- genDictEntryTypesAndEvaluatorForRecord dataConstructorName restOfFields
  let thisDictTypeE = [|($(nameToStringLiteralExpr fieldName), $(genDictEntryTypeFromHaskellType fieldType)) : $restDictType|]
  let thisEvaluatorE = [|$restEvaluator <*> $(genDictEntryEvaluatorForRecordField fieldName fieldType)|]
  pure (thisDictTypeE, thisEvaluatorE)

-- | Write a @DictEntryType@ that corresponds to a given a Haskell type.
genDictEntryTypeFromHaskellType :: Type -> ExpQ
genDictEntryTypeFromHaskellType typ =
  waspKindOfType typ >>= \case
    KOptional elemType -> [|T.DictOptional $(genWaspTypeFromHaskellType elemType)|]
    _ -> [|T.DictRequired $(genWaspTypeFromHaskellType typ)|]

-- | "genDictEvaluatorE fieldName typ" writes a "DictEvaluator" for a haskell record field
-- named "fieldName" with a value "typ".
genDictEntryEvaluatorForRecordField :: Name -> Type -> ExpQ
genDictEntryEvaluatorForRecordField fieldName fieldType =
  waspKindOfType fieldType >>= \case
    KOptional elemType -> [|maybeField $(nameToStringLiteralExpr fieldName) $(genEvaluatorExprForHaskellType elemType)|]
    _ -> [|field $(nameToStringLiteralExpr fieldName) $(genEvaluatorExprForHaskellType fieldType)|]

-- | An intermediate mapping between Haskell types and Wasp types, used for
-- generating @Types@, @Evaluator@, @DictEntryTypes@, and @DictEvaluator@.
data WaspKind
  = KString
  | KInteger
  | KDouble
  | KBool
  | KList Type
  | KImport
  | KJSON
  | KPSL
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
            | name == ''E.ExtImport -> pure KImport
            | name == ''E.JSON -> pure KJSON
            | name == ''E.PSL -> pure KPSL
          ListT `AppT` elemType -> pure (KList elemType)
          ConT name `AppT` elemType | name == ''Maybe -> pure (KOptional elemType)
          _ -> fail $ "No translation to wasp type for type " ++ show typ

-- ========================================
-- IsEnumType generation
-- ========================================

makeIsEnumTypeDefinition :: Name -> [Name] -> Q [DecQ]
makeIsEnumTypeDefinition typeName dataConstructorNames =
  pure
    [ func
        'enumType
        [|
          TD.EnumType
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

-- ========================================
-- Helper functions
-- ========================================

-- | Get an expression representing the string form of a name, starting with a lowercase letter
nameToLowerFirstStringLiteralExpr :: Name -> ExpQ
nameToLowerFirstStringLiteralExpr = litE . stringL . toLowerFirst . nameBase

-- | Get an expression representing the string form of a name
nameToStringLiteralExpr :: Name -> ExpQ
nameToStringLiteralExpr = litE . stringL . nameBase

-- | @func name expr@ writes a function like @name = expr@
func :: Name -> ExpQ -> DecQ
func name expr = funD name [clause [] (normalB expr) []]
