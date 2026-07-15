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

import Data.Aeson (FromJSON, ToJSON)
import Data.Data (Data)
import Data.List (intercalate)
import GHC.Generics (Generic)
import Wasp.AppSpec.Core.Inspectable (Inspectable (..), InspectionEntry (InspectionEntry))
import Wasp.AppSpec.Core.IsDecl (IsDecl)
import Wasp.AppSpec.Core.Ref (Ref, refName)
import Wasp.AppSpec.Entity (Entity)
import Wasp.AppSpec.ExtImport (ExtImport, showExtImport)

data Api = Api
  { fn :: ExtImport,
    middlewareConfigFn :: Maybe ExtImport,
    entities :: Maybe [Ref Entity],
    httpRoute :: (HttpMethod, String), -- (method, path), exe: (GET, "/foo/bar")
    auth :: Maybe Bool
  }
  deriving (Show, Eq, Data, Generic, FromJSON, ToJSON)

instance IsDecl Api

instance Inspectable Api where
  inspect api =
    [ InspectionEntry "API" $
        [ ("Method", show (method api)),
          ("Route", path api),
          ("Import", showExtImport $ fn api)
        ]
          ++ [("Entities", (intercalate ", " . fmap refName) entities') | Just entities' <- [entities api]]
          ++ [("Auth", "Enabled") | auth api == Just True]
    ]

method :: Api -> HttpMethod
method = fst . httpRoute

path :: Api -> String
path = snd . httpRoute

data HttpMethod = ALL | GET | POST | PUT | DELETE
  deriving (Show, Eq, Ord, Data, Generic, FromJSON, ToJSON)
