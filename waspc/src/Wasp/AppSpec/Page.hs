{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module Wasp.AppSpec.Page
  ( Page (..),
  )
where

import qualified Data.Aeson as Aeson
import Data.Data (Data)
import Data.Maybe (catMaybes)
import GHC.Generics (Generic)
import Wasp.AppSpec.Core.IsDecl (IsDecl)
import Wasp.AppSpec.ExtImport (ExtImport)
import Wasp.AppSpec.JSON (maybeToField)

data Page = Page
  { component :: ExtImport,
    authRequired :: Maybe Bool
  }
  deriving (Show, Eq, Data, Generic, Aeson.FromJSON)

instance IsDecl Page

instance Aeson.ToJSON Page where
  toJSON page =
    let requiredFields = ["component" Aeson..= component page]
        optionalFields =
          [ maybeToField "authRequired" (authRequired page)
          ]
     in Aeson.object (requiredFields <> catMaybes optionalFields)
