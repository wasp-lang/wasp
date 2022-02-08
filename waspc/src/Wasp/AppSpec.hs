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
import Wasp.AppSpec.Valid (Valid, fromValid, (<$^>))
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

getQueries :: AppSpec -> [(String, Query)]
getQueries = getDecls @Query

getActions :: AppSpec -> [(String, Action)]
getActions = getDecls @Action

getEntities :: AppSpec -> [(String, Entity)]
getEntities = getDecls @Entity

getPages :: AppSpec -> [(String, Page)]
getPages = getDecls @Page

getRoutes :: AppSpec -> [(String, Route)]
getRoutes = getDecls @Route

-- TODO: Move into Valid.AppSpec?
getApp :: Valid AppSpec -> Valid (String, App)
getApp spec =
  let apps = getDecls @App <$> spec
   in case fromValid apps of
        [_] -> head <$> apps
        apps' ->
          error $
            "Expected exactly 1 'app' declaration in Valid AppSpec, but found " ++ show (length apps')
              ++ ". This should never happen."

-- TODO: Make it work on App instead of AppSpec? Move it somewhere, maybe to Valid.AppSpec?
isAuthEnabled :: Valid AppSpec -> Bool
isAuthEnabled spec = isJust (App.auth $ snd <$^> getApp spec)
