module Wasp (
    Wasp (..),
    WaspElement (..),
) where

data Wasp = Wasp [WaspElement] deriving (Show)

data WaspElement
    = WaspElementApp App
    | WaspElementPage
    | WaspElementEntity
    deriving (Show)

data App = App
    { appName :: !String -- Identifier
    , appTitle :: !String -- Title
    } deriving (Show)
