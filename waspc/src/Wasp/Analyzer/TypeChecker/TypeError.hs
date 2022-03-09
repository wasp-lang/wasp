{-# LANGUAGE LambdaCase #-}

module Wasp.Analyzer.TypeChecker.TypeError
  ( TypeError (..),
    TypeError' (..),
    TypeCoercionErrorReason (..),
    TypeCoercionError (..),
    getErrorMessageAndCtx,
    mkTypeError,
  )
where

import Control.Arrow (first)
import Data.List (intercalate)
import Wasp.Analyzer.Parser.Ctx (Ctx)
import Wasp.Analyzer.Type
import Wasp.Analyzer.TypeChecker.AST
import Wasp.Util (concatPrefixAndText, concatShortPrefixAndText, indent)

newtype TypeError = TypeError (WithCtx TypeError')
  deriving (Show, Eq)

{- ORMOLU_DISABLE -}
data TypeError'
  -- | Type coercion error that occurs when trying to "unify" the type T1 of typed expression with some other type T2.
  -- If there is a super type that both T2 and T1 can be safely coerced to, "unify" will succeed, but if not,
  -- we get this error.
  -- We use "unify" in the TypeChecker when trying to infer the common type for typed expressions that we know
  -- should be of the same type (e.g. for elements in the list).
  = UnificationError    TypeCoercionError
  -- | Type coercion error that occurs when trying to "weaken" the typed expression from its type T1 to some type T2.
  -- If T2 is super type of T1 and T1 can be safely coerced to T2, "weaken" will succeed, but if not, we get this error.
  -- We use "weaken" in the TypeChecker when trying to match inferred type of typed expression with some expected type.
  | WeakenError         TypeCoercionError
  | NoDeclarationType   TypeName
  | UndefinedIdentifier Identifier
  | QuoterUnknownTag    QuoterTag
  | DictDuplicateField  DictFieldName
  deriving (Eq, Show)
{- ORMOLU_ENABLE -}

type TypeName = String

type QuoterTag = String

type DictFieldName = String

mkTypeError :: Ctx -> TypeError' -> TypeError
mkTypeError ctx e = TypeError $ WithCtx ctx e

getErrorMessageAndCtx :: TypeError -> (String, Ctx)
getErrorMessageAndCtx (TypeError (WithCtx ctx typeError)) = case typeError of
  (NoDeclarationType typeName) -> ("Unknown declaration type: " ++ typeName, ctx)
  (UndefinedIdentifier identifier) -> ("Undefined identifier: " ++ identifier, ctx)
  (QuoterUnknownTag quoterTag) -> ("Unknown quoter tag: " ++ quoterTag, ctx)
  (DictDuplicateField dictFieldName) -> ("Duplicate dictionary field: " ++ dictFieldName, ctx)
  (UnificationError e) -> getUnificationErrorMessageAndCtx e
  (WeakenError e) -> getWeakenErrorMessageAndCtx e

-- TypeCoercionError <typed expression> <type which we tried to coerce the typed expression to/with> <reason>
data TypeCoercionError = TypeCoercionError (WithCtx TypedExpr) Type (TypeCoercionErrorReason TypeCoercionError)
  deriving (Eq, Show)

-- | Describes a reason that a @UnificationError@ or @WeakenError@ happened
data TypeCoercionErrorReason e
  = -- | A coercion involving a DeclType and a different type happened. For example,
    -- @unifyTypes (DeclType "foo") (DeclType "bar")@ and
    -- @unifyTypes (DeclType "foo") StringType@ would use this reason.
    ReasonDecl
  | -- | A coercion involving an EnumType and a different type happened. Similar to
    -- @ReasonDecl@, but for enum types.
    ReasonEnum
  | -- | There is no relationship between the types in the coercion
    ReasonUncoercable
  | -- | A coercion of the type contained in a list failed
    ReasonList e
  | -- | A coercion failed because a dictionary was missing a key
    ReasonDictNoKey String
  | -- | A coercion failed because a dictionary contained an extra key
    ReasonDictExtraKey String
  | -- | A coercion failed because two dictionaries had uncoercable types for a key
    ReasonDictWrongKeyType String e
  deriving (Eq, Show)

second :: (b -> d) -> (a, b, c) -> (a, d, c)
second f (x, y, z) = (x, f y, z)

extractTypeErrorMessagesAndCtx :: (Type -> TypedExpr -> String) -> TypeCoercionError -> (String, [String], Ctx)
extractTypeErrorMessagesAndCtx getUncoercableTypesMsg (TypeCoercionError (WithCtx ctx texpr) t reason) =
  case reason of
    ReasonList e ->
      second ("-> In list element" :) $ extractTypeErrorMessagesAndCtx getUncoercableTypesMsg e
    ReasonDictWrongKeyType key e ->
      second (("-> In dictionary field '" ++ key ++ "'") :) $ extractTypeErrorMessagesAndCtx getUncoercableTypesMsg e
    ReasonDictNoKey key -> ("-> Missing required dictionary field '" ++ key ++ "'", [], ctx)
    ReasonDictExtraKey key -> ("-> Unexpected dictionary field '" ++ key ++ "'", [], ctx)
    ReasonDecl -> uncoercableTypesMsgAndCtx
    ReasonEnum -> uncoercableTypesMsgAndCtx
    ReasonUncoercable -> uncoercableTypesMsgAndCtx
  where
    uncoercableTypesMsgAndCtx = (getUncoercableTypesMsg t texpr, [], ctx)

joinAdditionalMessages :: [String] -> String
joinAdditionalMessages [] = ""
joinAdditionalMessages msgChain = foldr1 appendMsg $ reverse msgChain
  where
    appendMsg curr acc = intercalate ":\n" [curr, indent 2 acc]

getTypeCoercionErrorMessageAndCtx :: (Type -> TypedExpr -> String) -> TypeCoercionError -> (String, Ctx)
getTypeCoercionErrorMessageAndCtx getUncoercableTypesMsg typeCoercionError = (fullMsg, ctx)
  where
    (mainMsg, additionalMsgs, ctx) = extractTypeErrorMessagesAndCtx getUncoercableTypesMsg typeCoercionError
    fullMsg = intercalate "\n\n" [mainMsg, joinAdditionalMessages additionalMsgs]

getUnificationErrorMessageAndCtx :: TypeCoercionError -> (String, Ctx)
getUnificationErrorMessageAndCtx = getTypeCoercionErrorMessageAndCtx $
  \t texpr ->
    intercalate
      "\n"
      [ "Can't mix the following types:",
        concatShortPrefixAndText " - " (show t),
        concatShortPrefixAndText " - " (show $ exprType texpr)
      ]

getWeakenErrorMessageAndCtx :: TypeCoercionError -> (String, Ctx)
getWeakenErrorMessageAndCtx = getTypeCoercionErrorMessageAndCtx $
  \t texpr ->
    intercalate
      "\n"
      [ concatPrefixAndText "Expected type: " (show t),
        concatPrefixAndText "Actual type:   " (show $ exprType texpr)
      ]
