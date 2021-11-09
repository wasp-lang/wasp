module AppSpec
  ( AppSpec (..),
  )
where

-- TODO: Write documentation explaining that this module is the central IR (intermediate representation) of Wasp.

import AppSpec.Core.Decl (Decl)
import qualified AppSpec.ExternalCode as ExternalCode

data AppSpec = AppSpec
  { decls :: [Decl],
    externalCodeFiles :: ExternalCode.File
  }
