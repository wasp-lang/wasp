{-# LANGUAGE TupleSections #-}

module Analyzer.Evaluator.Decl.Operations
  ( takeDecls,
    makeDecl,
    fromDecl,
  )
where

import Analyzer.Evaluator.Decl.Internal
import Analyzer.TypeDefinitions (IsDeclType)
import Data.Maybe (mapMaybe)
import Data.Typeable (Typeable, cast)

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
takeDecls :: (Typeable a, IsDeclType a) => [Decl] -> [(String, a)]
takeDecls = mapMaybe fromDecl

makeDecl :: (Typeable a, IsDeclType a) => String -> a -> Decl
makeDecl = Decl

fromDecl :: (Typeable a, IsDeclType a) => Decl -> Maybe (String, a)
fromDecl (Decl name value) = (name,) <$> cast value
