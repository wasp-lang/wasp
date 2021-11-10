{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}

module Wasp.AppSpec.Core.Ref
  ( Ref (..),
  )
where

import Wasp.AppSpec.Core.Decl (IsDecl)
import Data.Data (Data)

-- | Reference to a part (declaration) of the app spec, by its name.
-- e.g. `Ref "HomePage" :: Ref Page` is a reference to a page that is declared under the name "HomePage".
-- newtype Ref a = Ref {name :: String} deriving (Show, Eq, Data)
data Ref a where
  Ref :: (IsDecl a) => String -> Ref a

deriving instance Eq a => Eq (Ref a)

deriving instance Show a => Show (Ref a)

deriving instance (IsDecl a, Data a) => Data (Ref a)
