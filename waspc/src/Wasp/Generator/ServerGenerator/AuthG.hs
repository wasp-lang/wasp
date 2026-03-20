module Wasp.Generator.ServerGenerator.AuthG
  ( genAuth,
    depsRequiredByAuth,
    jwtSecretEnvVarName,
  )
where

import Data.Aeson (object, (.=))
import Data.Maybe (fromJust)
import StrongPath
  ( Dir,
    File',
    Path,
    Path',
    Posix,
    Rel,
    reldirP,
    relfile,
    (</>),
  )
import qualified StrongPath as SP
import Wasp.AppSpec (AppSpec)
import qualified Wasp.AppSpec.App as AS.App
import qualified Wasp.AppSpec.App.Auth as AS.Auth
import Wasp.AppSpec.Valid (getApp)
import qualified Wasp.ExternalConfig.Npm.Dependency as Npm.Dependency
import Wasp.Generator.Auth.Provider
  ( OAuthProviderSpec (..),
    enabledOAuthProviders,
    isEmailEnabled,
    isOAuthEnabled,
    isUsernameAndPasswordEnabled,
  )
import Wasp.Generator.FileDraft (FileDraft)
import Wasp.Generator.JsImport (jsImportToImportJson)
import Wasp.Generator.Monad (Generator)
import Wasp.Generator.ServerGenerator.Auth.EmailAuthG (genEmailAuth)
import Wasp.Generator.ServerGenerator.Auth.LocalAuthG (genLocalAuth)
import Wasp.Generator.ServerGenerator.Auth.OAuthAuthG (genOAuthAuth)
import qualified Wasp.Generator.ServerGenerator.Common as C
import Wasp.Generator.ServerGenerator.JsImport (extImportToAliasedImportJson)
import qualified Wasp.JsImport as JI
import Wasp.Util ((<++>))

genAuth :: AppSpec -> Generator [FileDraft]
genAuth spec = case maybeAuth of
  Nothing -> return []
  Just auth ->
    sequence
      [ genAuthRoutesIndex auth,
        genFileCopy [relfile|routes/auth/me.ts|],
        genFileCopy [relfile|routes/auth/logout.ts|],
        genProvidersIndex auth,
        genAuthHooks auth
      ]
      <++> genLocalAuth auth
      <++> genOAuthAuth auth
      <++> genEmailAuth spec auth
  where
    maybeAuth = AS.App.auth $ snd $ getApp spec
    genFileCopy = return . C.mkSrcTmplFd

genAuthRoutesIndex :: AS.Auth.Auth -> Generator FileDraft
genAuthRoutesIndex auth = return $ C.mkTmplFdWithDstAndData tmplFile dstFile (Just tmplData)
  where
    tmplFile = C.srcDirInServerTemplatesDir </> SP.castRel authIndexFileInSrcDir
    dstFile = C.serverSrcDirInServerRootDir </> authIndexFileInSrcDir
    tmplData =
      object ["isExternalAuthEnabled" .= isOAuthEnabled auth]

    authIndexFileInSrcDir :: Path' (Rel C.ServerSrcDir) File'
    authIndexFileInSrcDir = [relfile|routes/auth/index.js|]

genProvidersIndex :: AS.Auth.Auth -> Generator FileDraft
genProvidersIndex auth = return $ C.mkTmplFdWithData [relfile|src/auth/providers/index.ts|] (Just tmplData)
  where
    tmplData =
      object
        [ "providers" .= providers,
          "isExternalAuthEnabled" .= isOAuthEnabled auth
        ]

    providers =
      makeConfigImportJson
        <$> concat
          [ [slug spec | (spec, _) <- enabledOAuthProviders auth],
            ["username" | isUsernameAndPasswordEnabled auth],
            ["email" | isEmailEnabled auth]
          ]

    makeConfigImportJson providerId =
      jsImportToImportJson $
        Just $
          JI.JsImport
            { JI._path = JI.RelativeImportPath $ [reldirP|./config|] </> (fromJust . SP.parseRelFileP $ providerId <> ".js"),
              JI._name = JI.JsImportModule providerId,
              JI._importAlias = Nothing
            }

genAuthHooks :: AS.Auth.Auth -> Generator FileDraft
genAuthHooks auth = return $ C.mkTmplFdWithData [relfile|src/auth/hooks.ts|] (Just tmplData)
  where
    tmplData =
      object
        [ "onBeforeSignupHook" .= onBeforeSignupHook,
          "onAfterSignupHook" .= onAfterSignupHook,
          "onAfterEmailVerifiedHook" .= onAfterEmailVerifiedHook,
          "onBeforeOAuthRedirectHook" .= onBeforeOAuthRedirectHook,
          "onBeforeLoginHook" .= onBeforeLoginHook,
          "onAfterLoginHook" .= onAfterLoginHook
        ]
    onBeforeSignupHook = extImportToAliasedImportJson "onBeforeSignupHook_ext" relPathToServerSrcDir $ AS.Auth.onBeforeSignup auth
    onAfterSignupHook = extImportToAliasedImportJson "onAfterSignupHook_ext" relPathToServerSrcDir $ AS.Auth.onAfterSignup auth
    onAfterEmailVerifiedHook = extImportToAliasedImportJson "onAfterEmailVerifiedHook_ext" relPathToServerSrcDir $ AS.Auth.onAfterEmailVerified auth
    onBeforeOAuthRedirectHook = extImportToAliasedImportJson "onBeforeOAuthRedirectHook_ext" relPathToServerSrcDir $ AS.Auth.onBeforeOAuthRedirect auth
    onBeforeLoginHook = extImportToAliasedImportJson "onBeforeLoginHook_ext" relPathToServerSrcDir $ AS.Auth.onBeforeLogin auth
    onAfterLoginHook = extImportToAliasedImportJson "onAfterLoginHook_ext" relPathToServerSrcDir $ AS.Auth.onAfterLogin auth

    relPathToServerSrcDir :: Path Posix (Rel importLocation) (Dir C.ServerSrcDir)
    relPathToServerSrcDir = [reldirP|../|]

depsRequiredByAuth :: AppSpec -> [Npm.Dependency.Dependency]
depsRequiredByAuth spec = maybe [] (const authDeps) maybeAuth
  where
    maybeAuth = AS.App.auth $ snd $ getApp spec
    authDeps =
      Npm.Dependency.fromList
        [ ("lucia", "^3.0.1"),
          ("@lucia-auth/adapter-prisma", "^4.0.0")
        ]

jwtSecretEnvVarName :: String
jwtSecretEnvVarName = "JWT_SECRET"
