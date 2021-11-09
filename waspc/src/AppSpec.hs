module AppSpec
  ( AppSpec (..),
  )
where

import AppSpec.Core.Decl (Decl)
import qualified AppSpec.ExternalCode as ExternalCode

data AppSpec = AppSpec
  { decls :: [Decl],
    externalCodeFiles :: ExternalCode.File
  }
