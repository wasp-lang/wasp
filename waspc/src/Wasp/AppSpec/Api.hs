{-# LANGUAGE DeriveDataTypeable #-}

module Wasp.AppSpec.Api
  ( Api (..),
    HttpMethod (..),
    method,
    path,
  )
where

import Data.Data (Data)
import Wasp.AppSpec.Core.Decl (IsDecl)
import Wasp.AppSpec.Core.Ref (Ref)
import Wasp.AppSpec.Entity (Entity)
import Wasp.AppSpec.ExtImport (ExtImport)

data Api = Api
  { fn :: ExtImport,
    entities :: Maybe [Ref Entity],
    httpRoute :: (HttpMethod, String),
    auth :: Maybe Bool
  }
  deriving (Show, Eq, Data)

instance IsDecl Api

method :: Api -> HttpMethod
method = fst . httpRoute

path :: Api -> String
path = snd . httpRoute

data HttpMethod = ALL | GET | POST | PUT | DELETE
  deriving (Show, Eq, Data)
