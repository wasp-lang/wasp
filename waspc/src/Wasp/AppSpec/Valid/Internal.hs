{-# LANGUAGE DeriveFunctor #-}

module Wasp.AppSpec.Valid.Internal where

newtype Valid a = Valid a
  deriving (Functor, Show, Eq)
