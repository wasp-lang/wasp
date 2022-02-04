{-# LANGUAGE TypeApplications #-}

module Wasp.AppSpec
  ( AppSpec (..),
    Decl,
    getDecls,
    takeDecls,
    Ref,
    refName,
    getApp,
    getActions,
    getQueries,
    getEntities,
    getPages,
    getRoutes,
    isAuthEnabled,
  )
where

import Data.Maybe (isJust)
import StrongPath (Abs, Dir, File', Path')
import Wasp.AppSpec.Action (Action)
import Wasp.AppSpec.App (App)
import qualified Wasp.AppSpec.App as App
import Wasp.AppSpec.Core.Decl (Decl, IsDecl, takeDecls)
import Wasp.AppSpec.Core.Ref (Ref, refName)
import Wasp.AppSpec.Entity (Entity)
import qualified Wasp.AppSpec.ExternalCode as ExternalCode
import Wasp.AppSpec.Page (Page)
import Wasp.AppSpec.Query (Query)
import Wasp.AppSpec.Route (Route)
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
    -- | Absolute path to the .env file in wasp project source. It contains env variables to be
    -- provided to the server only during the development.
    dotEnvFile :: Maybe (Path' Abs File'),
    -- | If true, it means project is being compiled for production/deployment -> it is being "built".
    -- If false, it means project is being compiled for development purposes (e.g. "wasp start").
    isBuild :: Bool
  }

-- TODO: Make this return "Named" declarations?
-- We would have something like NamedDecl or smth like that. Or at least have a @type Named@ or smth like that.
-- Or @WithName@ or just @Named@.
-- I like the best: `newtype Named a = Named (String, a)`
-- I created a github issue for it: https://github.com/wasp-lang/wasp/issues/426 .
getDecls :: IsDecl a => AppSpec -> [(String, a)]
getDecls = takeDecls . decls

-- TODO: This will fail with an error if there is no `app` declaration (because of `head`)!
--   However, returning a Maybe here would be PITA later in the code.
--   It would be cool instead if we had an extra step that somehow ensures that app exists and
--   throws nice error if it doesn't. Some step that validated AppSpec. Maybe we could
--   have a function that returns `Validated AppSpec` -> so basically smart constructor,
--   validates AppSpec and returns it wrapped with `Validated`,
--   I created a github issue for it: https://github.com/wasp-lang/wasp/issues/425 .
getApp :: AppSpec -> (String, App)
getApp spec = case takeDecls @App (decls spec) of
  [app] -> app
  apps ->
    error $
      "Compiler error: expected exactly 1 'app' declaration in your wasp code, but you have "
        ++ show (length apps)
        ++ "!"

getQueries :: AppSpec -> [(String, Query)]
getQueries spec = takeDecls @Query (decls spec)

getActions :: AppSpec -> [(String, Action)]
getActions spec = takeDecls @Action (decls spec)

getEntities :: AppSpec -> [(String, Entity)]
getEntities spec = takeDecls @Entity (decls spec)

getPages :: AppSpec -> [(String, Page)]
getPages spec = takeDecls @Page (decls spec)

getRoutes :: AppSpec -> [(String, Route)]
getRoutes spec = takeDecls @Route (decls spec)

isAuthEnabled :: AppSpec -> Bool
isAuthEnabled spec = isJust (App.auth $ snd $ getApp spec)
