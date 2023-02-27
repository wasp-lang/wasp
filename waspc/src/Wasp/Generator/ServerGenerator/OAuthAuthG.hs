module Wasp.Generator.ServerGenerator.OAuthAuthG
  ( genOAuthAuth,
    depsRequiredByPassport,
  )
where

import Data.Aeson (object, (.=))
import qualified Data.Aeson as Aeson
import Data.Maybe (fromJust, isJust)
import StrongPath
  ( Dir,
    File',
    Path,
    Path',
    Posix,
    Rel,
    reldir,
    reldirP,
    relfile,
    (</>),
  )
import qualified StrongPath as SP
import Wasp.AppSpec (AppSpec)
import qualified Wasp.AppSpec.App as AS.App
import qualified Wasp.AppSpec.App.Auth as AS.App.Auth
import qualified Wasp.AppSpec.App.Auth as AS.Auth
import qualified Wasp.AppSpec.App.Dependency as App.Dependency
import Wasp.AppSpec.Valid (getApp)
import Wasp.Generator.AuthProviders (gitHubAuthInfo, googleAuthInfo)
import Wasp.Generator.AuthProviders.OAuth (OAuthAuthInfo)
import qualified Wasp.Generator.AuthProviders.OAuth as OAuth
import Wasp.Generator.FileDraft (FileDraft)
import Wasp.Generator.Monad (Generator)
import Wasp.Generator.ServerGenerator.Common (ServerSrcDir)
import qualified Wasp.Generator.ServerGenerator.Common as C
import Wasp.Generator.ServerGenerator.JsImport (extImportToImportJson)
import Wasp.Util ((<++>))

genOAuthAuth :: AS.Auth.Auth -> Generator [FileDraft]
genOAuthAuth auth
  | AS.Auth.isExternalAuthEnabled auth =
      genOAuthHelpers
        <++> genOAuthProvider googleAuthInfo (AS.Auth.google . AS.Auth.methods $ auth)
        <++> genOAuthProvider gitHubAuthInfo (AS.Auth.gitHub . AS.Auth.methods $ auth)
  | otherwise = return []

genOAuthHelpers :: Generator [FileDraft]
genOAuthHelpers =
  sequence
    [ return $ C.mkSrcTmplFd [relfile|auth/providers/oauth/init.ts|],
      return $ C.mkSrcTmplFd [relfile|auth/providers/oauth/setupRouter.ts|]
    ]

genOAuthProvider ::
  OAuthAuthInfo ->
  Maybe AS.Auth.ExternalAuthConfig ->
  Generator [FileDraft]
genOAuthProvider authInfo maybeUserConfig
  | isJust maybeUserConfig =
      sequence
        [ genOAuthConfig authInfo maybeUserConfig $ [reldir|auth/providers/config|] </> providerTsFile
        ]
  | otherwise = return []
  where
    providerTsFile :: Path' (Rel ()) File'
    providerTsFile = fromJust $ SP.parseRelFile $ slug ++ ".ts"

    slug = OAuth.slug authInfo

genOAuthConfig ::
  OAuthAuthInfo ->
  Maybe AS.Auth.ExternalAuthConfig ->
  Path' (Rel ServerSrcDir) File' ->
  Generator FileDraft
genOAuthConfig authInfo maybeUserConfig pathToConfigDst = return $ C.mkTmplFdWithDstAndData tmplFile dstFile (Just tmplData)
  where
    tmplFile = C.srcDirInServerTemplatesDir </> [relfile|auth/providers/config/oauth.ts|]
    dstFile = C.serverSrcDirInServerRootDir </> pathToConfigDst
    tmplData =
      object
        [ "slug" .= OAuth.slug authInfo,
          "name" .= OAuth.displayName authInfo,
          "npmPackage" .= App.Dependency.name (OAuth.passportDependency authInfo),
          "oAuthConfigProps" .= getJsonForOAuthConfigProps authInfo,
          "configFn" .= extImportToImportJson relPathFromAuthConfigToServerSrcDir maybeConfigFn,
          "userFieldsFn" .= extImportToImportJson relPathFromAuthConfigToServerSrcDir maybeGetUserFieldsFn
        ]
    maybeConfigFn = AS.Auth.configFn =<< maybeUserConfig
    maybeGetUserFieldsFn = AS.Auth.getUserFieldsFn =<< maybeUserConfig

    relPathFromAuthConfigToServerSrcDir :: Path Posix (Rel importLocation) (Dir C.ServerSrcDir)
    relPathFromAuthConfigToServerSrcDir = [reldirP|../../../|]

getJsonForOAuthConfigProps :: OAuthAuthInfo -> [Aeson.Value]
getJsonForOAuthConfigProps authInfo =
  [ object
      [ "key" .= ("clientID" :: String),
        "value" .= ("process.env." ++ OAuth.clientIdEnvVarName authInfo)
      ],
    object
      [ "key" .= ("clientSecret" :: String),
        "value" .= ("process.env." ++ OAuth.clientSecretEnvVarName authInfo)
      ],
    object
      [ "key" .= ("scope" :: String),
        "value" .= OAuth.scopeStr authInfo
      ]
  ]

depsRequiredByPassport :: AppSpec -> [App.Dependency.Dependency]
depsRequiredByPassport spec =
  concat
    [ [App.Dependency.make ("passport", "0.6.0") | (AS.App.Auth.isExternalAuthEnabled <$> maybeAuth) == Just True],
      [OAuth.passportDependency googleAuthInfo | (AS.App.Auth.isGoogleAuthEnabled <$> maybeAuth) == Just True],
      [OAuth.passportDependency gitHubAuthInfo | (AS.App.Auth.isGitHubAuthEnabled <$> maybeAuth) == Just True]
    ]
  where
    maybeAuth = AS.App.auth $ snd $ getApp spec
