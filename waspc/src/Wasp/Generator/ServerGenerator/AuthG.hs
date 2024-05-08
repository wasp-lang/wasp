module Wasp.Generator.ServerGenerator.AuthG
  ( genAuth,
    depsRequiredByAuth,
  )
where

import Data.Aeson (object, (.=))
import Data.Maybe (fromJust)
import StrongPath
  ( File',
    Path',
    Rel,
    reldirP,
    relfile,
    (</>),
  )
import qualified StrongPath as SP
import Wasp.AppSpec (AppSpec)
import qualified Wasp.AppSpec.App as AS.App
import qualified Wasp.AppSpec.App.Auth as AS.Auth
import qualified Wasp.AppSpec.App.Dependency as AS.Dependency
import Wasp.AppSpec.Valid (getApp)
import Wasp.Generator.AuthProviders
  ( emailAuthProvider,
    gitHubAuthProvider,
    googleAuthProvider,
    keycloakAuthProvider,
    localAuthProvider,
  )
import qualified Wasp.Generator.AuthProviders.Email as EmailProvider
import qualified Wasp.Generator.AuthProviders.Local as LocalProvider
import qualified Wasp.Generator.AuthProviders.OAuth as OAuthProvider
import Wasp.Generator.FileDraft (FileDraft)
import Wasp.Generator.JsImport (jsImportToImportJson)
import Wasp.Generator.Monad (Generator)
import Wasp.Generator.ServerGenerator.Auth.EmailAuthG (genEmailAuth)
import Wasp.Generator.ServerGenerator.Auth.LocalAuthG (genLocalAuth)
import Wasp.Generator.ServerGenerator.Auth.OAuthAuthG (genOAuthAuth)
import qualified Wasp.Generator.ServerGenerator.Common as C
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
        genProvidersIndex auth
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
      object ["isExternalAuthEnabled" .= AS.Auth.isExternalAuthEnabled auth]

    authIndexFileInSrcDir :: Path' (Rel C.ServerSrcDir) File'
    authIndexFileInSrcDir = [relfile|routes/auth/index.js|]

genProvidersIndex :: AS.Auth.Auth -> Generator FileDraft
genProvidersIndex auth = return $ C.mkTmplFdWithData [relfile|src/auth/providers/index.ts|] (Just tmplData)
  where
    tmplData =
      object
        [ "providers" .= providers,
          "isExternalAuthEnabled" .= AS.Auth.isExternalAuthEnabled auth
        ]

    providers =
      makeConfigImportJson
        <$> concat
          [ [OAuthProvider.providerId gitHubAuthProvider | AS.Auth.isGitHubAuthEnabled auth],
            [OAuthProvider.providerId googleAuthProvider | AS.Auth.isGoogleAuthEnabled auth],
            [OAuthProvider.providerId keycloakAuthProvider | AS.Auth.isKeycloakAuthEnabled auth],
            [LocalProvider.providerId localAuthProvider | AS.Auth.isUsernameAndPasswordAuthEnabled auth],
            [EmailProvider.providerId emailAuthProvider | AS.Auth.isEmailAuthEnabled auth]
          ]

    makeConfigImportJson providerId =
      jsImportToImportJson $
        Just $
          JI.JsImport
            { JI._path = JI.RelativeImportPath $ [reldirP|./config|] </> (fromJust . SP.parseRelFileP $ providerId <> ".js"),
              JI._name = JI.JsImportModule providerId,
              JI._importAlias = Nothing
            }

depsRequiredByAuth :: AppSpec -> [AS.Dependency.Dependency]
depsRequiredByAuth spec = maybe [] (const authDeps) maybeAuth
  where
    maybeAuth = AS.App.auth $ snd $ getApp spec
    authDeps =
      AS.Dependency.fromList
        [ ("lucia", "^3.0.1"),
          ("@lucia-auth/adapter-prisma", "^4.0.0"),
          ("oslo", "^1.1.2")
        ]
