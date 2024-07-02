module Wasp.Valid
  ( ValidationError (..),
    isValidationError,
    isValidationWarning,
  )
where

data ValidationError = GenericValidationError !String | GenericValidationWarning !String
  deriving (Eq)

instance Show ValidationError where
  show (GenericValidationError e) = e
  show (GenericValidationWarning e) = e

isValidationError :: ValidationError -> Bool
isValidationError (GenericValidationError _) = True
isValidationError (GenericValidationWarning _) = False

isValidationWarning :: ValidationError -> Bool
isValidationWarning (GenericValidationError _) = False
isValidationWarning (GenericValidationWarning _) = True
