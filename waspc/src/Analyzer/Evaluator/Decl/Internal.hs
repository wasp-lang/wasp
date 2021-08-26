{-# LANGUAGE GADTs #-}

module Analyzer.Evaluator.Decl.Internal
  ( Decl (Decl),
  )
where

import Data.Typeable (Typeable)

-- | Used to store a heterogenous lists of evaluated declarations during
--   evaluation.
data Decl where
  -- | @Decl "Name" value@ results from a declaration statement "declType Name value".
  Decl :: (Typeable a) => String -> a -> Decl
