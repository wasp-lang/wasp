{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Wasp.Analyzer.TypeDefinitions.TH.Decl
  ( makeDeclType,
  )
where

import Control.Applicative ((<|>))
import qualified Data.HashMap.Strict as H
import Language.Haskell.TH
import Language.Haskell.TH.Syntax (VarBangType)
import qualified Wasp.Analyzer.Evaluator.Evaluation as E
import qualified Wasp.Analyzer.Type as T
import Wasp.Analyzer.TypeDefinitions (DeclType (..), EnumType (..), IsDeclType (..), IsEnumType (..))
import Wasp.Analyzer.TypeDefinitions.Class.HasCustomEvaluation (HasCustomEvaluation)
import qualified Wasp.Analyzer.TypeDefinitions.Class.HasCustomEvaluation as HasCustomEvaluation
import Wasp.Analyzer.TypeDefinitions.TH.Common
import Wasp.AppSpec.Core.Decl (makeDecl)
import Wasp.AppSpec.Core.Ref (Ref)
import qualified Wasp.AppSpec.ExtImport as AppSpec.ExtImport
import qualified Wasp.AppSpec.JSON as AppSpec.JSON

-- | @makeDeclType ''T@ generates an @IsDeclType@ instance for Haskell type @T@.
-- A type error is raised if @T@ does not fit the criteria described below, which are
-- required for @makeDeclType@ to be able to automatically generate @IsDeclType@
-- instance for a type @T@.
--
-- Requirements on the shape of type @T@ for this to work:
--   - type @T@ must be an ADT with one constructor, which must have
--     just one field OR use record syntax (in which case it can have multiple record fields).
--   - type @T@ must have no type parameters.
-- Therefore, some valid types based on this would be:
--   - `data T = Foo String`
--   - `data T = Foo { a :: String, b :: Int }`
-- while some invalid types would be:
--   - `data T = Foo String | Bar Int`
--   - `data T = Foo String Int`
--   - `data T a = Foo a`
--
-- Further, there are requirements/limitations as to which types can @T@ contain in itself
-- (be it as a type of the record field or as a type of the data constructor field).
-- They can be:
--   - Primitive: @String@, @Integer@, @Double@, @Bool@.
--   - @[a]@ where @a@ is an accepted type itself.
--   - @Maybe a@ (but only as a type of a record field) where @a@ is an accepted type itself.
--   - A few special types from AppSpec:
--     - 'Wasp.AppSpec.ExtImport.ExtImport'
--     - 'Wasp.AppSpec.JSON.JSON'
--     - @Wasp.AppSpec.Core.Ref a@ where @a@ is an instance of @IsDeclType@.
--   - A type that implements an @IsEnumType@ instance.
--   - A type with just one data constructor that is a record, and whose record field types
--     are all accepted types.
--   - A type that implements a @HasCustomEvaluation@ instance.
-- Place in implementation that is a source of truth for this is 'waspKindOfHaskellType'.
--
-- If all of the above requirements are met, @IsDeclType@ instance is generated.
-- Main parts of that instance are Wasp type ('Wasp.Analyzer.Type.Type')
-- of the declaration body (referred to as @WT@ from now on) and the evaluator
-- that evalutes from that Wasp type @WT@ into Haskell type @T@.
-- We can also describe @WT@ with @let WT = dtBodyType (declType \@T)@.
-- For the user of @makeDeclType@, it is important to understand how
-- @makeDeclType@ determines @WT@ based on the @T@, since @WT@ then determines
-- how will this new declaration type (that we described with @T@) manifest
-- in the Wasp language itself.
--
-- @WT@ is determined based on @T@ in the following manner:
--   - If @T@ is a record, then @WT@ is a @Dict@ with the same field keys as @T@ has,
--     and field types are determined as described below. recursively in this same manner.
--     Only exception is if type of field in record @T@ is @Maybe a@ -> in that case
--     type of corresponding field in @Dict@ @WT@ is determined from @a@ but field is
--     marked as optional (otherwise it is required).
--   - If @T@ is a simple ADT (not record) with just one field, then @WT@ is determined
--     directly from the type of that one field.
--   - When determining how types of fields (in record or in simple ADT) from @T@ map
--     to parts of @WT@, following approach is used:
--       - Primitives, list, ExtImport, JSON, enum, decl ref -> all those are pretty
--         straight-forwardly mapped onto corresponding types from `Wasp.Analyzer.Type`.
--       - Type of record field in @T@ maps to required field in @WT@ @Dict@ of
--         type that maps per rules mentioned above/below.
--         Only exception is if type of field in record @T@ is @Maybe a@ -> in that case
--         type of corresponding field in @Dict@ @WT@ is determined from @a@ but field is
--         marked as optional (otherwise it is required).
--       - If type of field is a record itself, then it is maped in a same way as
--         @T@ would be mapped -> as a @Dict@ with the same fields as the record.
--         Therefore this results in nested dictionaries.
--       - For types that have an instance of @HasCustomEvaluation@,
--         its @waspType@ method is directly used to obtain the Wasp type.
--         This allows us to take control over how Wasp type is determined for this specific
--         sub-part of @T@.
--
-- Finally, it is important to mention that name of the declaration type is the same
-- as the name of @T@ but with first letter changed to lowercase:
-- @dtName (declType \@MyType) == "myType"@
--
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
      instanceDefinition = makeIsDeclTypeInstanceDefinition typeName dataConstructor
  sequence [instanceDeclaration]

-- | Top-level "IsDeclType" instance generator.
makeIsDeclTypeInstanceDefinition :: Name -> Con -> Q [DecQ]
-- The constructor is in the form @data Type = Type x@
makeIsDeclTypeInstanceDefinition typeName _dataConstructor@(NormalC conName [(_, conType)]) =
  genIsDeclTypeInstanceDefinitionFromNormalDataConstructor typeName conName conType
-- The constructor is in the form @data Type = Type x1 x2 ... xn@, which is not valid for a decl
makeIsDeclTypeInstanceDefinition typeName _dataConstructor@(NormalC _ values) =
  fail $
    "makeDeclType expects given type " ++ show typeName
      ++ " to be a record or to have one data constructor with exactly 1 value, "
      ++ "but instead it was given a data constructor with "
      ++ show (length values)
      ++ "values."
-- The constructor is in the form @data Type = Type { k1 :: f1, ..., kn :: fn }
makeIsDeclTypeInstanceDefinition typeName _dataConstructor@(RecC conName fields) =
  genIsDeclTypeInstanceDefinitionFromRecordDataConstructor typeName conName (recordFieldsToNameTypePairs fields)
-- The constructor is in an unsupported form
makeIsDeclTypeInstanceDefinition _ _ = fail "makeDeclType expects given type to have a normal or record constructor"

recordFieldsToNameTypePairs :: [VarBangType] -> [(Name, Type)]
recordFieldsToNameTypePairs = map $ \(fieldName, _, fieldType) -> (fieldName, fieldType)

-- | Create an "IsDeclType" instance for types that have a single data constructor which has a single value,
-- e.g. @data Type = Type x@.
genIsDeclTypeInstanceDefinitionFromNormalDataConstructor :: Name -> Name -> Type -> Q [DecQ]
genIsDeclTypeInstanceDefinitionFromNormalDataConstructor typeName dataConstructorName dataConstructorType = do
  (bodyTypeE, evaluationE) <- genWaspTypeAndEvaluationForHaskellType dataConstructorType
  let evaluateE = [|E.runEvaluation $ $(conE dataConstructorName) <$> $evaluationE|]
  pure $ genIsDeclTypeInstanceDefinition typeName dataConstructorName bodyTypeE evaluateE

-- | For decls with record constructors, i.e. @data Fields = Fields { a :: String, b :: String }
genIsDeclTypeInstanceDefinitionFromRecordDataConstructor :: Name -> Name -> [(Name, Type)] -> Q [DecQ]
genIsDeclTypeInstanceDefinitionFromRecordDataConstructor typeName dataConstructorName fields = do
  (dictTypeE, dictEvaluationE) <- genDictWaspTypeAndEvaluationForRecord dataConstructorName fields
  let evaluateE = [|E.runEvaluation $dictEvaluationE|]
  let bodyTypeE = dictTypeE
  pure $ genIsDeclTypeInstanceDefinition typeName dataConstructorName bodyTypeE evaluateE

-- | Generates 'declType' function for a definition of IsDeclType instance.
-- A helper function for 'genPrimDecl' and 'genRecDecl'.
genIsDeclTypeInstanceDefinition :: Name -> Name -> ExpQ -> ExpQ -> [DecQ]
genIsDeclTypeInstanceDefinition typeName dataConstructorName bodyTypeE evaluateE =
  [ genFunc
      'declType
      [|
        DeclType
          { dtName = $(nameToLowerFirstStringLiteralExpr dataConstructorName),
            dtBodyType = $bodyTypeE,
            dtEvaluate = \typeDefs bindings declName declBodyExpr ->
              makeDecl @ $(conT typeName) declName <$> declEvaluate typeDefs bindings declBodyExpr
          }
        |],
    genFunc 'declEvaluate evaluateE
  ]

--------------- Kind, Wasp Type and Evaluation of a Haskell type ------------------

type WaspTypeExpQ = ExpQ

type EvaluationExpQ = ExpQ

-- | For a given Haskell type @typ@, generates two TH expressions:
-- one that is a Wasp @Type@, and another that is @Evaluation@ that evaluates from that Wasp @Type@
-- into a given Haskell type @typ@.
genWaspTypeAndEvaluationForHaskellType :: Type -> Q (WaspTypeExpQ, EvaluationExpQ)
genWaspTypeAndEvaluationForHaskellType typ =
  waspKindOfHaskellType typ >>= \case
    KString -> return ([|T.StringType|], [|E.string|])
    KInteger -> return ([|T.NumberType|], [|E.integer|])
    KDouble -> return ([|T.NumberType|], [|E.double|])
    KBool -> return ([|T.BoolType|], [|E.bool|])
    KList elemHaskellType -> do
      (elemWaspType, elemEvaluation) <- genWaspTypeAndEvaluationForHaskellType elemHaskellType
      return ([|T.ListType $(elemWaspType)|], [|E.list $(elemEvaluation)|])
    kt@(KTuple _) -> waspTypeAndEvaluationForTuple kt
    KImport -> return ([|T.ExtImportType|], [|E.extImport|])
    KJSON -> return ([|T.QuoterType "json"|], [|E.json|])
    KDeclRef t ->
      return
        ( [|T.DeclType $ dtName $ declType @ $(pure t)|],
          [|E.declRef @ $(pure t)|]
        )
    KEnum ->
      return
        ( [|T.EnumType $ etName $ enumType @ $(pure typ)|],
          [|E.enum @ $(pure typ)|]
        )
    KRecord dataConName fields -> genDictWaspTypeAndEvaluationForRecord dataConName fields
    KCustomEvaluation ->
      return
        ( [|HasCustomEvaluation.waspType @ $(pure typ)|],
          [|HasCustomEvaluation.evaluation @ $(pure typ)|]
        )
    KOptional _ -> fail "Maybe is only allowed in record fields"

waspTypeAndEvaluationForTuple :: WaspKind -> Q (WaspTypeExpQ, EvaluationExpQ)
waspTypeAndEvaluationForTuple = \case
  KTuple (t1, t2, []) -> do
    (wt1, e1) <- genWaspTypeAndEvaluationForHaskellType t1
    (wt2, e2) <- genWaspTypeAndEvaluationForHaskellType t2
    return
      ( [|T.TupleType ($(wt1), $(wt2), [])|],
        [|E.tuple2 $(e1) $(e2)|]
      )
  KTuple (t1, t2, [t3]) -> do
    (wt1, e1) <- genWaspTypeAndEvaluationForHaskellType t1
    (wt2, e2) <- genWaspTypeAndEvaluationForHaskellType t2
    (wt3, e3) <- genWaspTypeAndEvaluationForHaskellType t3
    return
      ( [|T.TupleType ($(wt1), $(wt2), [$(wt3)])|],
        [|E.tuple3 $(e1) $(e2) $(e3)|]
      )
  KTuple (t1, t2, [t3, t4]) -> do
    (wt1, e1) <- genWaspTypeAndEvaluationForHaskellType t1
    (wt2, e2) <- genWaspTypeAndEvaluationForHaskellType t2
    (wt3, e3) <- genWaspTypeAndEvaluationForHaskellType t3
    (wt4, e4) <- genWaspTypeAndEvaluationForHaskellType t4
    return
      ( [|T.TupleType ($(wt1), $(wt2), [$(wt3), $(wt4)])|],
        [|E.tuple4 $(e1) $(e2) $(e3) $(e4)|]
      )
  KTuple (_, _, _) -> fail "Only tuples of length 2, 3 or 4 are supported."
  _ -> error "This should never happen: function was called on kind that is not KTuple."

-- | Find the "WaspKind" of a Haskell type.
-- Wasp Kind is really just an intermediate representation that captures
-- information we will need to later determine Wasp type and Evaluation for
-- this Haskell type, which will be used to evaluate type-checked AST.
waspKindOfHaskellType :: Type -> Q WaspKind
waspKindOfHaskellType typ = do
  maybeDeclRefKind <- tryCastingToDeclRefKind typ
  maybeEnumKind <- tryCastingToEnumKind typ
  maybeCustomEvaluationKind <- tryCastingToCustomEvaluationKind typ
  maybeRecordKind <- tryCastingToRecordKind typ
  maybe (fail $ "No translation to wasp type for type " ++ show typ) return $
    maybeDeclRefKind
      <|> maybeEnumKind
      -- NOTE: It is important that @maybeCustomEvaluationKind@ is before @maybeRecordKind@,
      -- since having a custom evaluation should override typical record evalution, if type is a record.
      <|> maybeCustomEvaluationKind
      <|> maybeRecordKind
      <|> case typ of
        ConT name
          | name == ''String -> pure KString
          | name == ''Integer -> pure KInteger
          | name == ''Double -> pure KDouble
          | name == ''Bool -> pure KBool
          | name == ''AppSpec.ExtImport.ExtImport -> pure KImport
          | name == ''AppSpec.JSON.JSON -> pure KJSON
        ListT `AppT` elemType -> pure (KList elemType)
        ConT name `AppT` elemType | name == ''Maybe -> pure (KOptional elemType)
        TupleT 2 `AppT` t1 `AppT` t2 -> pure (KTuple (t1, t2, []))
        TupleT 3 `AppT` t1 `AppT` t2 `AppT` t3 -> pure (KTuple (t1, t2, [t3]))
        TupleT 4 `AppT` t1 `AppT` t2 `AppT` t3 `AppT` t4 -> pure (KTuple (t1, t2, [t3, t4]))
        _ -> Nothing
  where
    tryCastingToDeclRefKind :: Type -> Q (Maybe WaspKind)
    tryCastingToDeclRefKind (ConT name `AppT` subType) | name == ''Ref = do
      isDeclTypeRef <- isInstance ''IsDeclType [subType]
      return $ if isDeclTypeRef then Just (KDeclRef subType) else Nothing
    tryCastingToDeclRefKind _ = return Nothing

    tryCastingToEnumKind :: Type -> Q (Maybe WaspKind)
    tryCastingToEnumKind t = do
      isEnumType <- isInstance ''IsEnumType [t]
      return $ if isEnumType then Just KEnum else Nothing

    tryCastingToRecordKind :: Type -> Q (Maybe WaspKind)
    tryCastingToRecordKind (ConT typeName) = do
      (TyConI typeDeclaration) <- reify typeName
      return $ case typeDeclaration of
        (DataD _ _ [] _ [RecC dataConName fields] _) ->
          Just $ KRecord dataConName (recordFieldsToNameTypePairs fields)
        _ -> Nothing
    tryCastingToRecordKind _ = return Nothing

    tryCastingToCustomEvaluationKind :: Type -> Q (Maybe WaspKind)
    tryCastingToCustomEvaluationKind t = do
      hasInstance <- isInstance ''HasCustomEvaluation [t]
      return $ if hasInstance then Just KCustomEvaluation else Nothing

-- | An intermediate mapping between Haskell types and Wasp types, we use it internally
-- in this module when generating @Types@, @Evaluation@, @DictEntryTypes@, and @DictEvaluation@
-- so that we have easier time when figuring out what we are dealing with.
data WaspKind
  = KString
  | KInteger
  | KDouble
  | KBool
  | KList Type
  | KTuple (Type, Type, [Type])
  | KImport
  | KJSON
  | -- | Reference to a declaration type @Type@.
    KDeclRef Type
  | KEnum
  | -- | Valid only in a record field, represents @DictOptional@/@Maybe@
    KOptional Type
  | -- | Type that has a single data constructor that is a record.
    -- KRecord <record constructor name> <fields:(identifier, type)>
    KRecord Name [(Name, Type)]
  | KCustomEvaluation

---------- Kind, Wasp Type and Evaluation of a Haskell Record as a Wasp Dict -------------

-- | Given a record data constructor name and fields, return the evaluation that evaluates
-- Wasp dictionary into the given record, and also return the type of such Wasp dictionary.
-- First member of returned couple is type, second is evaluation.
genDictWaspTypeAndEvaluationForRecord :: Name -> [(Name, Type)] -> Q (WaspTypeExpQ, EvaluationExpQ)
genDictWaspTypeAndEvaluationForRecord dataConName fields = do
  (dictEntryTypesE, dictEvaluationE) <- genDictEntriesWaspTypeAndEvaluationForRecord dataConName fields
  return
    ( [|T.DictType $ H.fromList $dictEntryTypesE|],
      [|E.dict $dictEvaluationE|]
    )

genDictEntriesWaspTypeAndEvaluationForRecord :: Name -> [(Name, Type)] -> Q (WaspTypeExpQ, EvaluationExpQ)
genDictEntriesWaspTypeAndEvaluationForRecord dataConstructorName fields =
  go $ reverse fields -- Reversing enables us to apply evaluations in right order.
  where
    go [] = pure (listE [], [|pure|] `appE` conE dataConstructorName)
    go ((fieldName, fieldType) : restOfFields) = do
      (fieldWaspType, fieldEvaluation) <- genDictEntryWaspTypeAndEvaluationForRecordField fieldName fieldType
      (restWaspType, restEvaluation) <- go restOfFields
      pure
        ( [|($(nameToStringLiteralExpr fieldName), $fieldWaspType) : $restWaspType|],
          [|$restEvaluation <*> $fieldEvaluation|]
        )

-- | For a given Haskell record field (name and type), generate Wasp Type and
-- Evaluation from that Wasp Type into given record field.
genDictEntryWaspTypeAndEvaluationForRecordField :: Name -> Type -> Q (WaspTypeExpQ, EvaluationExpQ)
genDictEntryWaspTypeAndEvaluationForRecordField fieldName fieldType =
  waspKindOfHaskellType fieldType >>= \case
    KOptional elemType -> do
      (waspType, evaluation) <- genWaspTypeAndEvaluationForHaskellType elemType
      return
        ( [|T.DictOptional $(waspType)|],
          [|E.maybeField $(nameToStringLiteralExpr fieldName) $(evaluation)|]
        )
    _ -> do
      (waspType, evaluation) <- genWaspTypeAndEvaluationForHaskellType fieldType
      return
        ( [|T.DictRequired $(waspType)|],
          [|E.field $(nameToStringLiteralExpr fieldName) $(evaluation)|]
        )
