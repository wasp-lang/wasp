{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}

module Wasp.AppSpec.Core.Ref
  ( Ref (..),
    refName,
  )
where

import Data.Aeson (FromJSON, withObject, (.:))
import Data.Aeson.Types (FromJSON (parseJSON))
import Data.Data (Data)
import Wasp.AppSpec.Core.IsDecl (IsDecl (declTypeName))

-- | Reference to a part (declaration) of the app spec, by its name.
-- e.g. `Ref "HomePage" :: Ref Page` is a reference to a page that is declared under the name "HomePage".
data Ref a where
  Ref :: (IsDecl a) => String -> Ref a

deriving instance (Eq a) => Eq (Ref a)

deriving instance (Show a) => Show (Ref a)

deriving instance (IsDecl a, Data a) => Data (Ref a)

refName :: Ref a -> String
refName (Ref name) = name

instance (IsDecl a) => FromJSON (Ref a) where
  parseJSON = withObject ("Ref " <> declTypeName @a) $ \v -> do
    name <- v .: "name"
    declType <- v .: "declType"
    if declType == declTypeName @a
      then pure $ Ref name
      else
        fail $
          "Expected declType to be '"
            <> declTypeName @a
            <> "', but it was '"
            <> declType
            <> "'."
