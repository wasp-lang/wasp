{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module Analyzer.Evaluator.Decl
  ( Decl (Decl),
    takeDecls,
  )
where

import Data.Maybe (mapMaybe)
import Data.Typeable (Typeable, cast)

-- | Used to store a heterogenous lists of evaluated declarations during
--   evaluation.
data Decl where
  -- | @Decl "Name" value@ results from a declaration statement "declType Name value".
  Decl :: (Typeable a) => String -> a -> Decl

-- | Extracts all declarations of a certain type from a @[Decl]@s
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
takeDecls :: (Typeable a) => [Decl] -> [(String, a)]
takeDecls = mapMaybe $ \case
  Decl name value -> (name,) <$> cast value
