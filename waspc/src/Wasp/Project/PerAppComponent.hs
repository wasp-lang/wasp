{-# LANGUAGE DeriveTraversable #-}

module Wasp.Project.PerAppComponent where

-- | A value for each of the app components Wasp builds and runs: the client and
-- the server. See also 'Wasp.Generator.Common.GeneratedAppComponentDir'.
data PerAppComponent a = PerAppComponent
  { client :: a,
    server :: a
  }
  deriving (Show, Eq, Functor, Foldable, Traversable)

instance Applicative PerAppComponent where
  pure x = PerAppComponent x x
  PerAppComponent f1 f2 <*> PerAppComponent x1 x2 = PerAppComponent (f1 x1) (f2 x2)

appComponentNames :: PerAppComponent String
appComponentNames =
  PerAppComponent
    { client = "client",
      server = "server"
    }
