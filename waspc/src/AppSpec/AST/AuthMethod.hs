{-# LANGUAGE DeriveDataTypeable #-}

module AppSpec.AST.AuthMethod
  ( AuthMethod (..),
  )
where

import Data.Data (Data)

data AuthMethod = EmailAndPassword deriving (Show, Eq, Data)
