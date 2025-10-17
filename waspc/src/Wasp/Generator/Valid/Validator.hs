module Wasp.Generator.Valid.Validator where

import Data.Bifunctor (first)
import Data.List (intercalate)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.Maybe (maybeToList)
import Validation (validationToEither)
import qualified Validation as V
import Wasp.Util (indent)

type Validator input result = input -> Validation result

type Validation result =
  V.Validation (NonEmpty ValidationError) result

data ValidationError = ValidationError
  { message :: String,
    fieldPath :: [String],
    fileName :: Maybe String
  }

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

failure :: String -> Validation b
failure message' =
  V.failure $
    ValidationError
      { message = message',
        fieldPath = [],
        fileName = Nothing
      }

mapErrors :: (ValidationError -> ValidationError) -> Validation a -> Validation a
mapErrors = first . fmap

instance Show ValidationError where
  show
    ( ValidationError
        { message = message',
          fieldPath = fieldPath',
          fileName = fileName'
        }
      ) =
      unlines $
        maybeToList locationLine
          ++ [indent 4 message']
      where
        locationLine = case (fieldPath', fileName') of
          ([], Nothing) -> Nothing
          ([], Just fileName'') ->
            Just $
              "In " ++ show fileName'' ++ ":"
          (fieldPath'', Nothing) ->
            Just $
              "In " ++ show (intercalate "." fieldPath'') ++ ":"
          (fieldPath'', Just fileName'') ->
            Just $
              unwords
                [ "In",
                  show fileName'',
                  "→",
                  show (intercalate "." fieldPath'') ++ ":"
                ]
