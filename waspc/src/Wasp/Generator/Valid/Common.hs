{-# LANGUAGE FlexibleInstances #-}

module Wasp.Generator.Valid.Common where

import Control.Monad (void)
import Data.Bifunctor (first)
import Data.List (intercalate)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Validation (validationToEither)
import qualified Validation as V

type Validator input result = input -> Validation result

type Validation result =
  V.Validation (NonEmpty ValidationError) result

data ValidationError = ValidationError
  { message :: String,
    fieldPath :: [String],
    fileName :: Maybe String
  }

instance Show ValidationError where
  show
    ( ValidationError
        { message = message',
          fieldPath = fieldPath',
          fileName = fileName'
        }
      ) =
      unlines $
        message'
          : ( fieldLine fieldPath'
                ++ fileLine fileName'
            )
      where
        fieldLine [] = []
        fieldLine path = ["At " ++ show (intercalate "." path)]

        fileLine (Just name) = ["In " ++ show name]
        fileLine Nothing = []

runValidator :: Validator input result -> input -> Either (NonEmpty ValidationError) result
runValidator validator = validationToEither . validator

execValidator :: Validator input result -> input -> [ValidationError]
execValidator validator = either NE.toList (const []) . runValidator validator

fileValidator :: String -> Validator a result -> Validator a result
fileValidator fileName' innerValidator =
  mapErrors setFileName . innerValidator
  where
    setFileName err = err {fileName = Just fileName'}

field :: String -> (a -> b) -> Validator b result -> Validator a result
field fieldName fn check =
  mapErrors prependFieldName . check . fn
  where
    prependFieldName err = err {fieldPath = fieldName : fieldPath err}

mapErrors :: (ValidationError -> ValidationError) -> Validation a -> Validation a
mapErrors = first . fmap

eqJust :: (Eq a, Show a) => a -> Validator (Maybe a) ()
eqJust expected (Just actual) = eq expected actual
eqJust expected Nothing =
  failure $
    unwords
      [ "Missing value, expected:",
        show expected ++ "."
      ]

notPresent :: Validator (Maybe a) ()
notPresent Nothing = pure ()
notPresent (Just _) =
  failure "Field must be left unspecified."

eq :: (Eq a, Show a) => a -> Validator a ()
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

failure :: String -> Validation b
failure message' =
  V.failure $
    ValidationError
      { message = message',
        fieldPath = [],
        fileName = Nothing
      }

validateAll' :: [Validator a b] -> Validator a ()
validateAll' checks = void . V.validateAll checks
