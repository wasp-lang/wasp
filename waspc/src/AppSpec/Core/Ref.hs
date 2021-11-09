{-# LANGUAGE DeriveDataTypeable #-}

module AppSpec.Core.Ref
  ( Ref (..),
  )
where

import Data.Data (Data)

-- | Reference to a part (declaration) of the app spec, by its name.
-- e.g. `Ref "HomePage" :: Ref Page` is a reference to a page that is declared under the name "HomePage".
newtype Ref a = Ref {name :: String} deriving (Show, Eq, Data)
