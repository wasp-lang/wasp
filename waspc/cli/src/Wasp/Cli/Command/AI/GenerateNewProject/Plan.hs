{-# LANGUAGE DeriveGeneric #-}

module Wasp.Cli.Command.AI.GenerateNewProject.Plan
  ( Plan (..),
    Entity (..),
    Query (..),
    Action (..),
    Page (..),
  )
where

import Data.Aeson (FromJSON)
import GHC.Generics (Generic)

data Plan = Plan
  { entities :: [Entity],
    queries :: [Query],
    actions :: [Action],
    pages :: [Page]
  }
  deriving (Generic, Show)

instance FromJSON Plan

data Entity = Entity
  { entityName :: String,
    entityBodyPsl :: String,
    entityDesc :: String
  }
  deriving (Generic, Show)

instance FromJSON Entity

data Query = Query
  { queryName :: String,
    queryFnPath :: String,
    queryDesc :: String
  }
  deriving (Generic, Show)

instance FromJSON Query

data Action = Action
  { actionName :: String,
    actionFnPath :: String,
    actionDesc :: String
  }
  deriving (Generic, Show)

instance FromJSON Action

data Page = Page
  { pageName :: String,
    componentPath :: String,
    routePath :: String,
    pageDesc :: String
  }
  deriving (Generic, Show)

instance FromJSON Page
