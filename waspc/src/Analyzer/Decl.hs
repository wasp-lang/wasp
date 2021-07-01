{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module Analyzer.Decl
  ( Decl (..),
    takeDecls,
  )
where

import Analyzer.Lib (IsDecl)
import Data.Maybe (mapMaybe)
import Data.Typeable (Typeable, cast)

-- | Used to store a heterogenous lists of evaluated declarations during
--   evaluation.
data Decl where
  Decl :: (Typeable a, IsDecl a) => String -> a -> Decl

-- | Extracts all declarations of a certain type from a list of `Decl`s.
--
--  Example:
--
--  @
--  data Person = Person { name :: String, age :: Integer } deriving Generic
--  data Building = Building { address :: String } deriving Generic
--  let decls = [ Decl "Bob" $ Person "Bob" 42
--              , Decl "Office" $ Building "1 Road St"
--              , Decl "Alice" $ Person "Alice" 32
--              ]
--  takeDecls @Person decls == [("Bob", Person "Bob" 42), ("Alice", Person "Alice" 32)]
--  @
takeDecls :: (Typeable a, IsDecl a) => [Decl] -> [(String, a)]
takeDecls = mapMaybe $ \case
  Decl name value -> (name,) <$> cast value
