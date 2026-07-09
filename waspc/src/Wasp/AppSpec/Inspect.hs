{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}

module Wasp.AppSpec.Inspect
  ( InspectionSection (..),
    inspectAppSpec,
  )
where

import Wasp.AppSpec (AppSpec)
import qualified Wasp.AppSpec as AS
import Wasp.AppSpec.App (App)
import Wasp.AppSpec.Core.Inspectable (Inspectable (..), InspectionEntry (..))
import qualified Wasp.AppSpec.Valid as ASV
import qualified Wasp.Version

data InspectionSection = InspectionSection
  { sectionTitle :: String,
    sectionEntries :: [InspectionEntry]
  }
  deriving (Show, Eq)

-- | Brings together the self-descriptions of all declarations in the app spec,
-- in a fixed order. Entities are deliberately left out: routes and operations
-- are the point of `wasp inspect`, and entities are visible in schema.prisma.
inspectAppSpec :: AppSpec -> [InspectionSection]
inspectAppSpec spec =
  appSection spec :
  [ section (AS.getRoutes spec),
    section (AS.getPages spec),
    section (AS.getQueries spec),
    section (AS.getActions spec),
    section (AS.getApis spec),
    section (AS.getApiNamespaces spec),
    section (AS.getCruds spec),
    section (AS.getJobs spec)
  ]

-- | The app's own entry, extended with facts only the whole spec knows.
appSection :: AppSpec -> InspectionSection
appSection spec =
  InspectionSection
    (inspectionSection @App)
    [InspectionEntry $ entryCells (inspect $ ASV.getApp spec) ++ specContextCells]
  where
    specContextCells =
      [ "db: " ++ show (ASV.getValidDbSystem spec),
        "wasp " ++ show Wasp.Version.waspVersion
      ]

section :: forall a. (Inspectable a) => [(String, a)] -> InspectionSection
section namedDecls = InspectionSection (inspectionSection @a) (inspect <$> namedDecls)
