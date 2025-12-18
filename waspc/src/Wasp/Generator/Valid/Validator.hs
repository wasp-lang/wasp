module Wasp.Generator.Valid.Validator where

import Data.Bifunctor (first)
import Data.Functor (void)
import Data.List (intercalate)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.Maybe (catMaybes)
import qualified Validation as V
import Wasp.Util (indent)

type Validator input = input -> Validation

type Validation = V.Validation (NonEmpty ValidationError) ()

data ValidationError = ValidationError
  { message :: String,
    fieldPath :: [String],
    fileName :: Maybe String
  }
  deriving (Eq)

-- | Executes the given validator on the input and returns a list of validation errors. If there are
-- no errors, returns an empty list.
execValidator :: Validator input -> input -> [ValidationError]
execValidator validator value =
  V.validation NE.toList (const []) $ validator value

-- | Combines multiple validators into one that succeeds only if all of them succeed.
-- Accumulates errors.
all :: [Validator a] -> Validator a
all vs = void . V.validateAll vs

-- | Combines two validators into one that succeeds only if both succeed.
-- Short-circuits on the first failure.
and :: Validator input -> Validator input -> Validator input
and v1 v2 value
  | V.isFailure result1 = result1
  | otherwise = result2
  where
    result1 = v1 value
    result2 = v2 value

-- | Adds file name context to validation errors produced by the inner validator.
withFileName :: String -> Validator a -> Validator a
withFileName fileName' innerValidator =
  mapErrors setFileName . innerValidator
  where
    setFileName err = err {fileName = Just fileName'}

-- | Runs the validator on a specific field of the input, adding the field name to the error path.
inField :: (String, a -> b) -> Validator b -> Validator a
inField (fieldName, fieldGetter) innerValidator =
  mapErrors prependFieldName . innerValidator . fieldGetter
  where
    prependFieldName err = err {fieldPath = fieldName : fieldPath err}

mapErrors :: (ValidationError -> ValidationError) -> Validation -> Validation
mapErrors = first . fmap

success :: Validation
success = pure ()

failure :: String -> Validation
failure message' =
  V.failure $
    ValidationError
      { message = message',
        fieldPath = [],
        fileName = Nothing
      }

eqJust :: (Eq a, Show a) => a -> Validator (Maybe a)
eqJust expected (Just actual)
  | actual == expected = success
  | otherwise =
      failure $ "Expected " ++ show expected ++ " but got " ++ show actual ++ "."
eqJust expected Nothing =
  failure $ "Missing value, expected " ++ show expected ++ "."

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
