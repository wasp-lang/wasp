{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module Wasp.AppSpec.Task
  ( Task (..),
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

-- | An administrative task, run from the terminal with `wasp task run`.
data Task = Task
  { name :: String,
    fn :: ExtImport,
    entities :: Maybe [Ref Entity],
    description :: Maybe String
  }
  deriving (Show, Eq, Data, Generic, FromJSON, ToJSON)

instance IsDecl Task where
  declName = name

instance Inspectable Task where
  inspect task =
    [ InspectionEntry "Tasks" $
        [ ("Name", task.name),
          ("Import", showExtImportFromProjectDir task.fn)
        ]
          ++ [("Entities", (intercalate ", " . fmap refName) entities') | Just entities' <- [task.entities]]
          ++ [("Description", description') | Just description' <- [task.description]]
    ]
