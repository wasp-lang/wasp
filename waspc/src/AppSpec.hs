module AppSpec
  ( AppSpec (..),
  )
where

import AppSpec.AST.Core.Decl (Decl)
-- TODO: Move definition of ExternalCode.File to the AppSpec.ExternalCode?
--   So it doesn't rely on external modules and is self contained?
import qualified ExternalCode

data AppSpec = AppSpec
  { decls :: [Decl],
    externalCodeFiles :: ExternalCode.File
  }
