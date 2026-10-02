{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module Wasp.AppSpec.Action
  ( Action (..),
  )
where

import Data.Aeson (FromJSON, ToJSON)
import Data.Data (Data)
import Data.List (intercalate)
import GHC.Generics (Generic)
import Wasp.AppSpec.Core.IsDecl (IsDecl (..))
import Wasp.AppSpec.Core.Ref (Ref, refName)
import Wasp.AppSpec.Entity (Entity)
import Wasp.AppSpec.ExtImport (ExtImport, showExtImportFromProjectDir)
import Wasp.Inspectable (Inspectable (..), InspectionEntry (InspectionEntry))

data Action = Action
  { name :: String,
    fn :: ExtImport,
    entities :: Maybe [Ref Entity],
    auth :: Maybe Bool
  }
  deriving (Show, Eq, Data, Generic, FromJSON, ToJSON)

instance IsDecl Action where
  declName = name

instance Inspectable Action where
  inspect action =
    [ InspectionEntry "Actions" $
        [ ("Name", action.name),
          ("Import", showExtImportFromProjectDir action.fn)
        ]
          ++ [("Entities", (intercalate ", " . fmap refName) entities') | Just entities' <- [action.entities]]
          ++ [("Auth", "Enabled") | action.auth == Just True]
    ]
