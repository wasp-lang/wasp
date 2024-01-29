module Wasp.Generator.ServerGenerator.Auth.OAuthAuthG
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
import qualified Wasp.AppSpec as AS
import qualified Wasp.AppSpec.App as AS.App
import qualified Wasp.AppSpec.App.Auth as AS.App.Auth
import qualified Wasp.AppSpec.App.Auth as AS.Auth
import qualified Wasp.AppSpec.App.Dependency as App.Dependency
import Wasp.AppSpec.Valid (getApp)
import Wasp.Generator.AuthProviders (gitHubAuthProvider, googleAuthProvider)
import Wasp.Generator.AuthProviders.OAuth (OAuthAuthProvider)
import qualified Wasp.Generator.AuthProviders.OAuth as OAuth
import qualified Wasp.Generator.DbGenerator.Auth as DbAuth
import Wasp.Generator.FileDraft (FileDraft)
import Wasp.Generator.Monad (Generator)
import Wasp.Generator.ServerGenerator.Common (ServerSrcDir)
import qualified Wasp.Generator.ServerGenerator.Common as C
import Wasp.Generator.ServerGenerator.JsImport (extImportToImportJson)
import Wasp.Util ((<++>))
import qualified Wasp.Util as Util

genOAuthAuth :: AS.Auth.Auth -> Generator [FileDraft]
genOAuthAuth auth
  | AS.Auth.isExternalAuthEnabled auth =
      genOAuthHelpers auth
        <++> genOAuthProvider googleAuthProvider (AS.Auth.google . AS.Auth.methods $ auth)
        <++> genOAuthProvider gitHubAuthProvider (AS.Auth.gitHub . AS.Auth.methods $ auth)
  | otherwise = return []

genOAuthHelpers :: AS.Auth.Auth -> Generator [FileDraft]
genOAuthHelpers auth =
  sequence
    [ genCreateRouter auth,
      genTypes auth,
      return $ C.mkSrcTmplFd [relfile|auth/providers/oauth/init.ts|]
    ]

genCreateRouter :: AS.Auth.Auth -> Generator FileDraft
genCreateRouter auth = return $ C.mkTmplFdWithData [relfile|src/auth/providers/oauth/createRouter.ts|] (Just tmplData)
  where
    tmplData =
      object
        [ "userEntityUpper" .= userEntityName,
          "authEntityUpper" .= (DbAuth.authEntityName :: String),
          "authIdentityEntityLower" .= (Util.toLowerFirst DbAuth.authIdentityEntityName :: String),
          "identitiesFieldOnAuthEntityName" .= (DbAuth.identitiesFieldOnAuthEntityName :: String),
          "authFieldOnAuthIdentityEntityName" .= (DbAuth.authFieldOnAuthIdentityEntityName :: String),
          "userFieldOnAuthEntityName" .= (DbAuth.userFieldOnAuthEntityName :: String)
        ]
    userEntityName = AS.refName . AS.Auth.userEntity $ auth

genTypes :: AS.Auth.Auth -> Generator FileDraft
genTypes auth = return $ C.mkTmplFdWithData tmplFile (Just tmplData)
  where
    tmplFile = C.srcDirInServerTemplatesDir </> [relfile|auth/providers/oauth/types.ts|]
    tmplData = object ["userEntityName" .= userEntityName]
    userEntityName = AS.refName $ AS.Auth.userEntity auth

genOAuthProvider ::
  OAuthAuthProvider ->
  Maybe AS.Auth.ExternalAuthConfig ->
  Generator [FileDraft]
genOAuthProvider provider maybeUserConfig
  | isJust maybeUserConfig =
      sequence
        [ genOAuthConfig provider maybeUserConfig $ [reldir|auth/providers/config|] </> providerTsFile
        ]
  | otherwise = return []
  where
    providerTsFile :: Path' (Rel ()) File'
    providerTsFile = fromJust $ SP.parseRelFile $ providerId ++ ".ts"

    providerId = OAuth.providerId provider

-- Used to generate the specific provider config based on the generic oauth.ts file.
-- The config receives everything: auth info, npm packages, user defined imports and env variables.
-- It's all in one config file.
genOAuthConfig ::
  OAuthAuthProvider ->
  Maybe AS.Auth.ExternalAuthConfig ->
  Path' (Rel ServerSrcDir) File' ->
  Generator FileDraft
genOAuthConfig provider maybeUserConfig pathToConfigDst = return $ C.mkTmplFdWithDstAndData tmplFile dstFile (Just tmplData)
  where
    tmplFile = C.srcDirInServerTemplatesDir </> [relfile|auth/providers/config/_oauth.ts|]
    dstFile = C.serverSrcDirInServerRootDir </> pathToConfigDst
    tmplData =
      object
        [ "providerId" .= OAuth.providerId provider,
          "displayName" .= OAuth.displayName provider,
          "npmPackage" .= App.Dependency.name (OAuth.passportDependency provider),
          "oAuthConfigProps" .= getJsonForOAuthConfigProps provider,
          "configFn" .= extImportToImportJson relPathFromAuthConfigToServerSrcDir maybeConfigFn,
          "userSignupFields" .= extImportToImportJson relPathFromAuthConfigToServerSrcDir maybeUserSignupFields
        ]
    maybeConfigFn = AS.Auth.configFn =<< maybeUserConfig
    maybeUserSignupFields = AS.Auth.userSignupFieldsForExternalAuth =<< maybeUserConfig

    relPathFromAuthConfigToServerSrcDir :: Path Posix (Rel importLocation) (Dir C.ServerSrcDir)
    relPathFromAuthConfigToServerSrcDir = [reldirP|../../../|]

getJsonForOAuthConfigProps :: OAuthAuthProvider -> [Aeson.Value]
getJsonForOAuthConfigProps provider =
  [ object
      [ "key" .= ("clientID" :: String),
        "value" .= ("process.env." ++ OAuth.clientIdEnvVarName provider)
      ],
    object
      [ "key" .= ("clientSecret" :: String),
        "value" .= ("process.env." ++ OAuth.clientSecretEnvVarName provider)
      ],
    object
      [ "key" .= ("scope" :: String),
        "value" .= OAuth.scopeStr provider
      ]
  ]

depsRequiredByPassport :: AppSpec -> [App.Dependency.Dependency]
depsRequiredByPassport spec =
  concat
    [ [App.Dependency.make ("passport", "0.6.0") | (AS.App.Auth.isExternalAuthEnabled <$> maybeAuth) == Just True],
      [OAuth.passportDependency googleAuthProvider | (AS.App.Auth.isGoogleAuthEnabled <$> maybeAuth) == Just True],
      [OAuth.passportDependency gitHubAuthProvider | (AS.App.Auth.isGitHubAuthEnabled <$> maybeAuth) == Just True]
    ]
  where
    maybeAuth = AS.App.auth $ snd $ getApp spec
