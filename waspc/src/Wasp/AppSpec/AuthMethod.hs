{-# LANGUAGE DeriveDataTypeable #-}

-- TODO: Move under App.Auth ?
module Wasp.AppSpec.AuthMethod
  ( AuthMethod (..),
  )
where

import Data.Data (Data)

data AuthMethod = EmailAndPassword deriving (Show, Eq, Data)
