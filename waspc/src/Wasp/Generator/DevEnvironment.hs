module Wasp.Generator.DevEnvironment
  ( DevEnvironment (..),
  )
where

data DevEnvironment = DevEnvironment
  { managedDatabaseUrl :: Maybe String
  }
