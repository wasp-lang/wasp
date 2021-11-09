{-# LANGUAGE DeriveDataTypeable #-}

module AppSpec.AST.Domain.AuthMethod
  ( AuthMethod (..),
  )
where

import Data.Data (Data)

data AuthMethod = EmailAndPassword deriving (Show, Eq, Data)
