{-# LANGUAGE FlexibleInstances #-}

module Wasp.Generator.Valid.Common where

import Control.Monad (void)
import Data.Bifunctor (first)
import Data.List (intercalate)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Validation (validationToEither)
import qualified Validation as V

type Validator input error result = input -> Validation error result

type Validation error result =
  V.Validation (NonEmpty (WithValidationContext error)) result

data WithValidationContext a = WithValidationContext ValidationContext a

instance Functor WithValidationContext where
  fmap f (WithValidationContext ctx a) = WithValidationContext ctx (f a)

mapContext :: (ValidationContext -> ValidationContext) -> WithValidationContext a -> WithValidationContext a
mapContext fn (WithValidationContext ctx a) = WithValidationContext (fn ctx) a

data ValidationContext = ValidationContext
  { fieldPath :: [String],
    fileName :: Maybe String
  }

emptyValidationContext :: ValidationContext
emptyValidationContext = ValidationContext [] Nothing

withEmptyValidationContext :: a -> WithValidationContext a
withEmptyValidationContext = WithValidationContext emptyValidationContext

instance Show (WithValidationContext String) where
  show
    ( WithValidationContext
        ValidationContext {fieldPath = fieldPath', fileName = fileName'}
        value
      ) =
      unlines $
        value
          : ( fieldLine fieldPath'
                ++ fileLine fileName'
            )
      where
        fieldLine [] = []
        fieldLine path = ["At " ++ show (intercalate "." path)]

        fileLine (Just name) = ["In " ++ show name]
        fileLine Nothing = []

runValidator :: Show error => Validator input error result -> input -> Either (NonEmpty (WithValidationContext error)) result
runValidator validator = validationToEither . validator

execValidator :: Show error => Validator input error result -> input -> [WithValidationContext error]
execValidator validator = either NE.toList (const []) . runValidator validator

fileValidator :: String -> Validator a error result -> Validator a error result
fileValidator fileName' innerValidator =
  mapErrorContexts setFileName . innerValidator
  where
    setFileName err = err {fileName = Just fileName'}

field :: String -> (a -> b) -> Validator b error result -> Validator a error result
field fieldName fn check =
  mapErrorContexts prependFieldName . check . fn
  where
    prependFieldName err = err {fieldPath = fieldName : fieldPath err}

field' :: String -> (a -> b) -> Validator b error result -> Validator a error ()
field' fieldName fn check value = void $ field fieldName fn check value

mapErrorContexts :: (ValidationContext -> ValidationContext) -> Validation e a -> Validation e a
mapErrorContexts fn = first (fmap $ mapContext fn)

eqJust :: (Eq a, Show a) => a -> Validator (Maybe a) String ()
eqJust expected (Just actual) = eq expected actual
eqJust expected Nothing =
  failure $
    unwords
      [ "Missing value, expected:",
        show expected ++ "."
      ]

notPresent :: Validator (Maybe a) String ()
notPresent Nothing = pure ()
notPresent (Just _) =
  failure "Field must be left unspecified."

eq :: (Eq a, Show a) => a -> Validator a String ()
eq expected actual
  | actual == expected = pure ()
  | otherwise =
      failure $
        unwords
          [ "Expected",
            show expected,
            "but got:",
            show actual ++ "."
          ]

failure :: a -> Validation a b
failure = V.failure . withEmptyValidationContext

validateAll' :: [Validator a e b] -> Validator a e ()
validateAll' checks = void . V.validateAll checks
