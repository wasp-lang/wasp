module Wasp (
  Wasp
) where

data Wasp = Wasp [WaspElement]

data WaspElement =
  App {
    appName :: String, -- Identifier.
    appTitle :: String
  } |
  Page |
  Entity
