module Wasp.AppSpec
  ( AppSpec (..),
  )
where

import StrongPath (Abs, Dir, File', Path')
import Wasp.AppSpec.Core.Decl (Decl)
import qualified Wasp.AppSpec.ExternalCode as ExternalCode
import Wasp.Common (DbMigrationsDir)

-- | AppSpec is the main/central intermediate representation (IR) of the whole Wasp compiler,
-- describing the web app specification with all the details needed to generate it.
-- It is standalone and de-coupled from other parts of the compiler and knows nothing about them,
-- instead other parts are using it: Analyzer produces AppSpec while Generator consumes it.
data AppSpec = AppSpec
  { -- | List of declarations like App, Page, Route, ... that describe the web app.
    decls :: [Decl],
    -- | List of external code files (they are referenced/used by the declarations).
    externalCodeFiles :: [ExternalCode.File],
    -- | Absolute path to the directory in wasp project source that contains external code files.
    externalCodeDirPath :: !(Path' Abs (Dir ExternalCode.SourceExternalCodeDir)),
    -- | Absolute path to the directory in wasp project source that contains database migrations.
    migrationsDir :: Maybe (Path' Abs (Dir DbMigrationsDir)),
    dotEnvFile :: Maybe (Path' Abs File'),
    isBuild :: Bool
  }
