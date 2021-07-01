{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}

-- Todo:
-- When the Analyzer is finished, these types should be moved to the `Wasp`
-- module.

module Analyzer.StdLib
  ( AuthMethod (..),
    App (..),
    stdLib,
  )
where

import qualified Analyzer.Lib as Lib
import GHC.Generics (Generic)

data AuthMethod = EmailAndPassword deriving (Generic)

data App = App {title :: String, authMethod :: AuthMethod} deriving (Generic)

-- | A Wasp Library containing all of the standard types required for Wasp to
--   work.
stdLib :: Lib.Lib
stdLib =
  -- addEnumType @AuthMethod $
  -- addDeclType @App $
  Lib.empty
