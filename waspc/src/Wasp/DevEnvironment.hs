module Wasp.DevEnvironment
  ( DevEnvironment (..),
  )
where

data DevEnvironment = DevEnvironment
  { managedDatabaseUrl :: Maybe String
  }
