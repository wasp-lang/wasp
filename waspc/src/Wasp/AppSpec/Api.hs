{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module Wasp.AppSpec.Api
  ( Api (..),
    HttpMethod (..),
    method,
    path,
  )
where

import qualified Data.Aeson as Aeson
import Data.Data (Data)
import Data.Maybe (catMaybes)
import GHC.Generics (Generic)
import Wasp.AppSpec.Core.IsDecl (IsDecl)
import Wasp.AppSpec.Core.Ref (Ref)
import Wasp.AppSpec.Entity (Entity)
import Wasp.AppSpec.ExtImport (ExtImport)
import Wasp.AppSpec.JSON (maybeToField)

data Api = Api
  { fn :: ExtImport,
    middlewareConfigFn :: Maybe ExtImport,
    entities :: Maybe [Ref Entity],
    httpRoute :: (HttpMethod, String), -- (method, path), exe: (GET, "/foo/bar")
    auth :: Maybe Bool
  }
  deriving (Show, Eq, Data, Generic, Aeson.FromJSON)

instance IsDecl Api

instance Aeson.ToJSON Api where
  toJSON api =
    let requiredFields =
          [ "fn" Aeson..= fn api,
            "httpRoute" Aeson..= httpRoute api
          ]
        optionalFields =
          [ maybeToField "middlewareConfigFn" (middlewareConfigFn api),
            maybeToField "entities" (entities api),
            maybeToField "auth" (auth api)
          ]
     in Aeson.object (requiredFields <> catMaybes optionalFields)

method :: Api -> HttpMethod
method = fst . httpRoute

path :: Api -> String
path = snd . httpRoute

data HttpMethod = ALL | GET | POST | PUT | DELETE
  deriving (Show, Eq, Ord, Data, Generic, Aeson.FromJSON, Aeson.ToJSON)
