module Wasp.Generator.ServerGenerator.AuthG
  ( genAuth,
    depsRequiredByAuth,
  )
where

import StrongPath (relfile)
import Wasp.AppSpec (AppSpec)
import qualified Wasp.AppSpec.App as AS.App
import Wasp.AppSpec.Valid (getApp)
import qualified Wasp.ExternalConfig.Npm.Dependency as Npm.Dependency
import Wasp.Generator.FileDraft (FileDraft)
import Wasp.Generator.Monad (Generator)
import qualified Wasp.Generator.ServerGenerator.Common as C

-- | Only the framework-owned auth routes are generated here: `/auth/me`,
-- `/auth/logout` and the credential exchange `/auth/login/:providerId`.
-- Every provider's own flows (Wasp's own auth included) are the routes its
-- manifest declared, mounted at that manifest's basePath.
genAuth :: AppSpec -> Generator [FileDraft]
genAuth spec = case maybeAuth of
  Nothing -> return []
  Just _ ->
    sequence
      [ genFileCopy [relfile|routes/auth/index.js|],
        genFileCopy [relfile|routes/auth/me.ts|],
        genFileCopy [relfile|routes/auth/logout.ts|],
        genFileCopy [relfile|routes/auth/login.ts|]
      ]
  where
    maybeAuth = AS.App.auth $ snd $ getApp spec
    genFileCopy = return . C.mkSrcTmplFd

depsRequiredByAuth :: AppSpec -> [Npm.Dependency.Dependency]
depsRequiredByAuth spec = case maybeAuth of
  Nothing -> []
  -- Wasp's session store backs every provider -- every login ends in a Wasp
  -- session -- so its dependencies are installed whenever auth is on.
  Just _ -> authDeps
  where
    maybeAuth = AS.App.auth $ snd $ getApp spec
    authDeps =
      Npm.Dependency.fromList
        [ ("lucia", "^3.0.1"),
          ("@lucia-auth/adapter-prisma", "^4.0.0")
        ]
