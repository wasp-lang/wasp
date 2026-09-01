module Wasp.Generator.ServerGenerator.AuthG
  ( genAuth,
    depsRequiredByAuth,
    jwtSecretEnvVarName,
  )
where

import Data.Aeson (object, (.=))
import StrongPath
  ( File',
    Path',
    Rel,
    relfile,
    (</>),
  )
import qualified StrongPath as SP
import Wasp.AppSpec (AppSpec)
import qualified Wasp.AppSpec.App as AS.App
import qualified Wasp.AppSpec.App.Auth as AS.Auth
import Wasp.AppSpec.Valid (getApp)
import qualified Wasp.ExternalConfig.Npm.Dependency as Npm.Dependency
import Wasp.Generator.FileDraft (FileDraft)
import Wasp.Generator.Monad (Generator)
import qualified Wasp.Generator.ServerGenerator.Common as C
import Wasp.Util ((<++>))

-- | Only the framework-owned auth routes are generated here: `/auth/me`,
-- `/auth/logout` and the credential exchange. Wasp's own signup and login
-- flows live in the @wasp.sh/auth lib, mounted at `/auth` as that
-- provider's route handler like any adapter package's routes.
genAuth :: AppSpec -> Generator [FileDraft]
genAuth spec = case maybeAuth of
  Nothing -> return []
  Just auth ->
    sequence
      [ genAuthRoutesIndex auth,
        genFileCopy [relfile|routes/auth/me.ts|],
        genFileCopy [relfile|routes/auth/logout.ts|]
      ]
      -- The credential exchange route: external providers establish a Wasp
      -- session by trading the provider's credential for one. Wasp's own auth
      -- mints sessions from its login flows instead.
      <++> onlyUnderExternalProvider auth (sequence [genFileCopy [relfile|routes/auth/login.ts|]])
  where
    maybeAuth = AS.App.auth $ snd $ getApp spec
    genFileCopy = return . C.mkSrcTmplFd

    onlyUnderExternalProvider auth gen
      | AS.Auth.isExternalAuthProviderUsed auth = gen
      | otherwise = return []

genAuthRoutesIndex :: AS.Auth.Auth -> Generator FileDraft
genAuthRoutesIndex auth = return $ C.mkTmplFdWithDstAndData tmplFile dstFile (Just tmplData)
  where
    tmplFile = C.srcDirInServerTemplatesDir </> SP.castRel authIndexFileInSrcDir
    dstFile = C.serverSrcDirInServerRootDir </> authIndexFileInSrcDir
    tmplData =
      object
        [ "isExternalAuthEnabled" .= AS.Auth.isExternalAuthEnabled auth,
          "isExternalAuthProviderUsed" .= AS.Auth.isExternalAuthProviderUsed auth
        ]

    authIndexFileInSrcDir :: Path' (Rel C.ServerSrcDir) File'
    authIndexFileInSrcDir = [relfile|routes/auth/index.js|]

depsRequiredByAuth :: AppSpec -> [Npm.Dependency.Dependency]
depsRequiredByAuth spec = case maybeAuth of
  Nothing -> []
  -- Wasp's session store backs every provider -- external logins are exchanged
  -- for a Wasp session -- so its dependencies are installed whenever auth is on.
  Just _ -> authDeps
  where
    maybeAuth = AS.App.auth $ snd $ getApp spec
    authDeps =
      Npm.Dependency.fromList
        [ ("lucia", "^3.0.1"),
          ("@lucia-auth/adapter-prisma", "^4.0.0")
        ]

jwtSecretEnvVarName :: String
jwtSecretEnvVarName = "JWT_SECRET"
