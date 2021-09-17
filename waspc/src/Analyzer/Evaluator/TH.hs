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

import Analyzer.Evaluator.Decl.Operations (makeDecl)
import Analyzer.Evaluator.Evaluation
import Analyzer.Evaluator.EvaluationError
import qualified Analyzer.Evaluator.Types as E
import qualified Analyzer.Type as T
import Analyzer.TypeDefinitions.Class
import Analyzer.TypeDefinitions.Type as TD
import qualified Data.HashMap.Strict as H
import Language.Haskell.TH
import Util (toLowerFirst)

-- | @makeDeclType ''Type@ writes an @IsDeclType@ instance for @Type@. A type
-- error is raised if @Type@ does not fit the criteria described below.
--
-- Requirements on @Type@ for this to work:
--  - The type must be an ADT with one constructor.
--  - The type must have just one field OR use record syntax (in which case it can have multiple fields).
--
-- Properties that hold for @Type@ and its generated @IsDeclType@ instance:
--   - For the rest of the bullet points below, let's say that
--     @let bodyType = dtBodyType (declType \@Type)@.
--   - If @Type@ uses record syntax, then
--     - @bodyType@ is a @Dict@
--     - If and only if there is a key @x@ in @bodyType@,
--       then @Type@ has a record @x@ with the same type.
--     - If a key @x@ is optional, then the record @x@ in @Type@ is a @Maybe@.
--   - If @Type@ is a simple ADT (not record) with one field, then @bodyType@ maps to the type of that field.
--   - @dtName (declType \@Type) == "type"@ -> the name of declaration type is the same as the name of @Type@
--     but with the first letter changed to lowercase.
--
-- One way to summarize the crux of properties above is to say: If given @Type@ is a simple ADT with one field,
-- it is translated into a declaration which has body corresponding to that field. If instead @Type@
-- is a record, it is translated into a declaration whose body is a dictionary with same fields as the record.
--
-- __Examples__
--
-- __Calling @makeDeclType@ on record__
-- @
-- >>> data User = User { name :: String, email :: Maybe String }
-- >>> makeDeclType ''User   -- "IsDeclType User" instance is generated.
-- >>> dtName $ declType @User
-- "user"
-- >>> dtBodyType $ declType @User
-- DictType [DictEntry "name" StringType, DictOptionalEntry "email" StringLiteral]
-- @
--
-- Such declaration type would be used in Wasp lang source somewhat like:
-- @
-- user MyUser { name: "testUser", email: "testuser@wasp-lang.dev" }
-- @
--
--
-- __Calling @makeDeclType@ on a simple ADT with one field__
-- @
-- >>> data Admins = Admins [User]
-- >>> makeDeclType ''Admins  -- "IsDeclType Admins" instance is generated.
-- >>> dtBodyType $ declType @Admins
-- ListType (DeclType "User")
-- @
--
-- Such declaration type would be used in Wasp lang source somewhat like:
-- @
-- admins MainAdmins [MyUser, SomeOtherUser]
-- @
makeDeclType :: Name -> Q [Dec]
makeDeclType typeName = do
  (TyConI typeDeclaration) <- reify typeName
  dataConstructor <- case typeDeclaration of
    (DataD _ _ [] _ [dataConstructor] _) -> pure dataConstructor
    DataD {} -> fail "makeDeclType expects a type declared with `data` to have exactly one constructor and no type variables."
    (NewtypeD _ _ [] _ dataConstructor _) -> pure dataConstructor
    NewtypeD {} -> fail "makeDeclType expects a type declared with `newtype` to have no type variables."
    _ -> fail "makeDeclType expects the given name to be for a type declared with `data` or `newtype`"
  let instanceDeclaration = instanceD instanceContext instanceType =<< instanceDefinition
      instanceContext = pure []
      instanceType = [t|IsDeclType $(conT typeName)|]
      instanceDefinition = makeIsDeclTypeInstanceDefinition dataConstructor
  sequence [instanceDeclaration]

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
  let evaluateE = [|runEvaluation $ $(conE dataConstructorName) <$> $(genEvaluationExprForHaskellType dataConstructorType)|]
  let bodyTypeE = genWaspTypeFromHaskellType dataConstructorType
  pure [genDeclTypeFuncOfIsDeclTypeInstance dataConstructorName bodyTypeE evaluateE]

-- | For decls with record constructors, i.e. @data Fields = Fields { a :: String, b :: String }
genIsDeclTypeInstanceDefinitionFromRecordDataConstructor :: Name -> [(Name, Type)] -> Q [DecQ]
genIsDeclTypeInstanceDefinitionFromRecordDataConstructor dataConstructorName fields = do
  (dictEntryTypesE, dictEvaluationE) <- genDictEntryTypesAndEvaluationForRecord dataConstructorName fields
  let evaluateE = [|runEvaluation $ dict $dictEvaluationE|]
  let bodyTypeE = [|T.DictType $ H.fromList $dictEntryTypesE|]
  pure [genDeclTypeFuncOfIsDeclTypeInstance dataConstructorName bodyTypeE evaluateE]

-- | Generates 'declType' function for a definition of IsDeclType instance.
-- A helper function for 'genPrimDecl' and 'genRecDecl'.
genDeclTypeFuncOfIsDeclTypeInstance :: Name -> ExpQ -> ExpQ -> DecQ
genDeclTypeFuncOfIsDeclTypeInstance dataConstructorName bodyTypeE evaluateE =
  genFunc
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

-- | Generates an expression that is @Evaluation@ that evaluates to a given Haskell type.
genEvaluationExprForHaskellType :: Type -> ExpQ
genEvaluationExprForHaskellType typ =
  waspKindOfType typ >>= \case
    KString -> [|string|]
    KInteger -> [|integer|]
    KDouble -> [|double|]
    KBool -> [|bool|]
    KList elemType -> [|list $(genEvaluationExprForHaskellType elemType)|]
    KImport -> [|extImport|]
    KJSON -> [|json|]
    KPSL -> [|psl|]
    KDecl -> [|decl @ $(pure typ)|]
    KEnum -> [|enum @ $(pure typ)|]
    KOptional _ -> fail "Maybe is only allowed in record fields"

-- | Write the @DictEntryType@s and @DictEvaluation@ for the Haskell record with given data constructor name and fields.
genDictEntryTypesAndEvaluationForRecord :: Name -> [(Name, Type)] -> Q (ExpQ, ExpQ)
genDictEntryTypesAndEvaluationForRecord dataConstructorName fields =
  go $ reverse fields -- Reversing enables us to apply evaluations in right order.
  where
    go [] = pure (listE [], varE 'pure `appE` conE dataConstructorName)
    go ((fieldName, fieldType) : restOfFields) = do
      (restDictType, restEvaluation) <- go restOfFields
      let thisDictTypeE =
            [|
              ($(nameToStringLiteralExpr fieldName), $(genDictEntryTypeFromHaskellType fieldType)) :
              $restDictType
              |]
      let thisEvaluationE = [|$restEvaluation <*> $(genDictEntryEvaluationForRecordField fieldName fieldType)|]
      pure (thisDictTypeE, thisEvaluationE)

-- | Write a @DictEntryType@ that corresponds to a given a Haskell type.
genDictEntryTypeFromHaskellType :: Type -> ExpQ
genDictEntryTypeFromHaskellType typ =
  waspKindOfType typ >>= \case
    KOptional elemType -> [|T.DictOptional $(genWaspTypeFromHaskellType elemType)|]
    _ -> [|T.DictRequired $(genWaspTypeFromHaskellType typ)|]

-- | "genDictEvaluationE fieldName typ" writes a "DictEvaluation" for a haskell record field
-- named "fieldName" with a value "typ".
genDictEntryEvaluationForRecordField :: Name -> Type -> ExpQ
genDictEntryEvaluationForRecordField fieldName fieldType =
  waspKindOfType fieldType >>= \case
    KOptional elemType -> [|maybeField $(nameToStringLiteralExpr fieldName) $(genEvaluationExprForHaskellType elemType)|]
    _ -> [|field $(nameToStringLiteralExpr fieldName) $(genEvaluationExprForHaskellType fieldType)|]

-- | An intermediate mapping between Haskell types and Wasp types, used for
-- generating @Types@, @Evaluation@, @DictEntryTypes@, and @DictEvaluation@.
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
    [ genFunc
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

-- | @genFunc name expr@ writes a function like @name = expr@
genFunc :: Name -> ExpQ -> DecQ
genFunc name expr = funD name [clause [] (normalB expr) []]
