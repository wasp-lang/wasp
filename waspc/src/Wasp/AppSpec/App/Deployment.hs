{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Wasp.AppSpec.App.Deployment
  ( Deployment (..),
    DeploymentMode (..),
  )
where

import Data.Aeson (FromJSON (parseJSON), ToJSON (toJSON), Value (String), withText)
import Data.Data (Data)
import GHC.Generics (Generic)

data Deployment = Deployment
  { mode :: Maybe DeploymentMode
  }
  deriving (Show, Eq, Data, Generic, FromJSON, ToJSON)

data DeploymentMode = Split
  deriving (Show, Eq, Data, Generic)

instance FromJSON DeploymentMode where
  parseJSON = withText "DeploymentMode" $ \deploymentMode ->
    case deploymentMode of
      "split" -> pure Split
      _ -> fail $ "Unknown deployment mode: " ++ show deploymentMode

instance ToJSON DeploymentMode where
  toJSON Split = String "split"
