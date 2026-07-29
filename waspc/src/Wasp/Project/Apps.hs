{-# LANGUAGE DeriveTraversable #-}

module Wasp.Project.Apps where

data Apps a = Apps
  { client :: a,
    server :: a
  }
  deriving (Show, Eq, Functor, Foldable, Traversable)

instance Applicative Apps where
  pure x = Apps x x
  Apps f1 f2 <*> Apps x1 x2 = Apps (f1 x1) (f2 x2)

names :: Apps String
names =
  Apps
    { client = "client",
      server = "server"
    }
