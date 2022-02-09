{-# LANGUAGE DeriveFunctor #-}

module Wasp.AppSpec.Valid.Internal where

newtype Valid a = MakeValid a
  deriving (Functor, Show, Eq)
