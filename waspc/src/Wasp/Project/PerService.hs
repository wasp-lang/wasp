{-# LANGUAGE DeriveTraversable #-}

module Wasp.Project.PerService where

data PerService a = PerService
  { client :: a,
    server :: a
  }
  deriving (Show, Eq, Functor, Foldable, Traversable)

instance Applicative PerService where
  pure x = PerService x x
  PerService f1 f2 <*> PerService x1 x2 = PerService (f1 x1) (f2 x2)

names :: PerService String
names =
  PerService
    { client = "client",
      server = "server"
    }
