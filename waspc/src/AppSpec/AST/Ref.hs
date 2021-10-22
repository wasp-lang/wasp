{-# LANGUAGE DeriveDataTypeable #-}

module AppSpec.AST.Ref
  ( Ref (..),
  )
where

import Data.Data (Data)

-- | Reference to a part of the AST, by its name.
-- e.g. `Ref "HomePage" :: Ref Page` is a reference to a page that is declared under the name "HomePage".
newtype Ref a = Ref {name :: String} deriving (Show, Eq, Data)
