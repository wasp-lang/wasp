module Wasp.Generator.Valid.Validator where

import Data.Bifunctor (first)
import Data.List (intercalate)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.Maybe (catMaybes)
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

-- | Runs the given validator on the input and returns either validation errors or the result.
runValidator :: Validator input result -> input -> Either (NonEmpty ValidationError) result
runValidator validator = V.validationToEither . validator

-- | Executes the given validator on the input and returns a list of validation errors. If there are
-- no errors, returns an empty list.
execValidator :: Validator input result -> input -> [ValidationError]
execValidator validator = either NE.toList (const []) . runValidator validator

-- | Adds file name context to validation errors produced by the inner validator.
inFile :: String -> Validator a result -> Validator a result
inFile fileName' innerValidator =
  mapErrors setFileName . innerValidator
  where
    setFileName err = err {fileName = Just fileName'}

-- | Runs the validator on a specific field of the input, adding the field name to the error path.
inField :: String -> (a -> b) -> Validator b result -> Validator a result
inField fieldName fn innerValidator =
  mapErrors prependFieldName . innerValidator . fn
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
      )
      | null context = message'
      | otherwise = unlines [contextLine, indent 4 message']
      where
        contextLine = "In " ++ intercalate " → " context ++ ":"
        context = catMaybes [fileNamePart, fieldPathPart]

        fileNamePart = show <$> fileName'

        fieldPathPart
          | null fieldPath' = Nothing
          | otherwise = Just $ show $ intercalate "." fieldPath'
