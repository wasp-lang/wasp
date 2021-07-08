{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}

-- Todo:
-- When the Analyzer is finished, these types should be moved to the `Wasp`
-- module.

module Analyzer.StdTypes
  ( AuthMethod (..),
    App (..),
    stdTypes,
  )
where

import qualified Analyzer.TypeDefinitions as TD
import GHC.Generics (Generic)

data AuthMethod = EmailAndPassword deriving (Generic)

data App = App {title :: String, authMethod :: AuthMethod} deriving (Generic)

-- | A Wasp Library containing all of the standard types required for Wasp to
--   work.
stdTypes :: TD.TypeDefinitions
stdTypes =
  -- addEnumType @AuthMethod $
  -- addDeclType @App $
  TD.empty
