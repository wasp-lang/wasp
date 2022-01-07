{-# LANGUAGE TypeApplications #-}

module Wasp.AppSpec
  ( AppSpec (..),
    getApp,
    getDecls,
  )
where

import StrongPath (Abs, Dir, File', Path')
import Wasp.AppSpec.App (App)
import Wasp.AppSpec.Core.Decl (Decl, IsDecl, takeDecls)
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
    migrationDir :: Maybe (Path' Abs (Dir DbMigrationsDir)),
    dotEnvFile :: Maybe (Path' Abs File'),
    isBuild :: Bool
  }

-- TODO: Should this be here or in AppSpec.App?
-- TODO: What if this fails? Returning a Maybe here would be PITA later in the code.
--   But would be cool if we had an extra step that somehow ensures that app exists and
--   throws nice error if it doesn't. Some step that validated AppSpec. Maybe we could
--   have a function that returns `Validated AppSpec` -> so basically smart constructor,
--   validates AppSpec and returns it wrapped with `Validated`,
--   and then functions like `getApp` would be executed on `Validated AppSpec`, so we can be sure
--   they will not return an error (sure in the sense that we know `Validated` checked some stuff already).
--   For this we should create `newtype ValidAppSpec = ValidAppSpec AppSpec` (or some better name?) and then
--   function `validateAppSpec :: AppSpec -> ValidAppSpec` and hide constructor `ValidAppSpec`.
--   But then we will be passing `ValidAppSpec` all around the Generator hm, is that ok?
getApp :: AppSpec -> (String, App)
getApp spec = head $ takeDecls @App (decls spec)

-- TODO: Make this return "Named" declarations? I already have a comment for this somewhere else I think.
-- We would have something like NamedDecl or smth like that. Or at least have a @type Named@ or smth like that.
-- Or @WithName@ or just @Named@.
getDecls :: IsDecl a => AppSpec -> [(String, a)]
getDecls = takeDecls . decls

-- TODO: re-export refName from here, for ease of access?
