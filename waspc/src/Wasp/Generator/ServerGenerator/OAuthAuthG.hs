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
    Dir',
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
import Wasp.Generator.AuthProviders.OAuth (ExternalAuthInfo, templateFilePathInPassportDir)
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
  ExternalAuthInfo ->
  Maybe AS.Auth.ExternalAuthConfig ->
  Generator [FileDraft]
genOAuthProvider authInfo maybeUserConfig
  | isJust maybeUserConfig =
      sequence
        [ genOAuthConfig authInfo $ [reldir|auth/providers/config|] </> providerTsFile,
          return $ C.mkSrcTmplFd $ OAuth.passportTemplateFilePath authInfo,
          return $ C.mkSrcTmplFd $ [reldir|auth/passport|] </> providerRelDir </> [relfile|defaults.js|],
          return $
            mkUserConfigForAuthProvider
              [relfile|auth/passport/generic/configMapping.js|]
              ([reldir|auth/passport|] </> providerRelDir </> [relfile|configMapping.js|])
              (Just userConfigJson)
        ]
  | otherwise = return []
  where
    providerRelDir :: Path' (Rel ()) Dir'
    providerRelDir = fromJust $ SP.parseRelDir slug

    providerTsFile :: Path' (Rel ()) File'
    providerTsFile = fromJust $ SP.parseRelFile $ slug ++ ".ts"

    slug = OAuth.slug authInfo
    userConfigJson = getJsonForUserConfig maybeUserConfig

genOAuthConfig ::
  ExternalAuthInfo ->
  Path' (Rel ServerSrcDir) File' ->
  Generator FileDraft
genOAuthConfig authInfo pathToConfigDst = return $ C.mkTmplFdWithDstAndData tmplFile dstFile (Just tmplData)
  where
    tmplFile = C.srcDirInServerTemplatesDir </> [relfile|auth/providers/config/oauth.ts|]
    dstFile = C.serverSrcDirInServerRootDir </> pathToConfigDst
    tmplData =
      object
        [ "slug" .= OAuth.slug authInfo,
          "npmPackage" .= App.Dependency.name (OAuth.passportDependency authInfo),
          "passportConfigImport" .= SP.fromRelFile ([reldir|../../passport/|] </> templateFilePathInPassportDir authInfo)
        ]

mkUserConfigForAuthProvider ::
  Path' (Rel C.ServerTemplatesSrcDir) File' ->
  Path' (Rel C.ServerSrcDir) File' ->
  Maybe Aeson.Value ->
  FileDraft
mkUserConfigForAuthProvider pathInTemplatesSrcDir pathInGenProjectSrcDir tmplData =
  C.mkTmplFdWithDstAndData srcPath dstPath tmplData
  where
    srcPath = C.srcDirInServerTemplatesDir </> pathInTemplatesSrcDir
    dstPath = C.serverSrcDirInServerRootDir </> pathInGenProjectSrcDir

getJsonForUserConfig :: Maybe AS.Auth.ExternalAuthConfig -> Aeson.Value
getJsonForUserConfig maybeUserConfig =
  object
    [ "configFn" .= extImportToImportJson relPathFromAuthConfigToServerSrcDir maybeConfigFn,
      "userFieldsFn" .= extImportToImportJson relPathFromAuthConfigToServerSrcDir maybeGetUserFieldsFn
    ]
  where
    maybeConfigFn = AS.Auth.configFn =<< maybeUserConfig
    maybeGetUserFieldsFn = AS.Auth.getUserFieldsFn =<< maybeUserConfig

    relPathFromAuthConfigToServerSrcDir :: Path Posix (Rel importLocation) (Dir C.ServerSrcDir)
    relPathFromAuthConfigToServerSrcDir = [reldirP|../../../|]

depsRequiredByPassport :: AppSpec -> [App.Dependency.Dependency]
depsRequiredByPassport spec =
  concat
    [ [App.Dependency.make ("passport", "0.6.0") | (AS.App.Auth.isExternalAuthEnabled <$> maybeAuth) == Just True],
      [OAuth.passportDependency googleAuthInfo | (AS.App.Auth.isGoogleAuthEnabled <$> maybeAuth) == Just True],
      [OAuth.passportDependency gitHubAuthInfo | (AS.App.Auth.isGitHubAuthEnabled <$> maybeAuth) == Just True]
    ]
  where
    maybeAuth = AS.App.auth $ snd $ getApp spec
