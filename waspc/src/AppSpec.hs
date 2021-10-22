module AppSpec
  ( AppSpec (..),
  )
where

import AppSpec.AST.Decl (Decl)
-- TODO: Move definiton of ExternalCode.File to the AppSpec?
--   So it doesn't rely on external modules and is self contained?
import qualified ExternalCode

data AppSpec = AppSpec
  { decls :: [Decl],
    externalCodeFiles :: ExternalCode.File
  }

-- TODO: Module organization:
-- AppSpec.AST.App  OR AppSpec.AST.Domain.App
-- AppSpec.AST.Ref  OR  AppSpec.AST.Core.Ref
-- AppSpec.AST.Decl  OR AppSpec.AST.Core.Decl
-- AppSpec.ExternalCode
--
