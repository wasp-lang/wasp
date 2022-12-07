module Wasp.Generator.ServerGenerator.ExternalAuthG
  ( genPassportAuth,
    depsRequiredByPassport,
  )
where

import Data.Aeson (object, (.=))
import qualified Data.Aeson as Aeson
import Data.Maybe (fromJust, fromMaybe, isJust)
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
import qualified Wasp.AppSpec.App.Auth as AS.App.Auth
import qualified Wasp.AppSpec.App.Auth as AS.Auth
import qualified Wasp.AppSpec.App.Dependency as App.Dependency
import Wasp.AppSpec.Valid (getApp)
import Wasp.Generator.ExternalCodeGenerator.Common (GeneratedExternalCodeDir)
import Wasp.Generator.FileDraft (FileDraft)
import Wasp.Generator.JsImport (getJsImportDetailsForExtFnImport)
import Wasp.Generator.Monad (Generator)
import qualified Wasp.Generator.ServerGenerator.Common as C
import Wasp.Generator.ServerGenerator.ExternalCodeGenerator (extServerCodeDirInServerSrcDir)
import Wasp.Util ((<++>))

genPassportAuth :: AS.Auth.Auth -> Generator [FileDraft]
genPassportAuth auth
  | AS.Auth.isExternalAuthEnabled auth =
      sequence
        [ genPassportJs auth,
          copyTmplFile [relfile|routes/auth/passport/generic/provider.js|]
        ]
        <++> genGoogleAuth auth
        <++> genGitHubAuth auth
  | otherwise = return []
  where
    copyTmplFile = return . C.mkSrcTmplFd

genPassportJs :: AS.Auth.Auth -> Generator FileDraft
genPassportJs auth = return $ C.mkTmplFdWithDstAndData tmplFile dstFile (Just tmplData)
  where
    tmplFile = C.srcDirInServerTemplatesDir </> SP.castRel passportFileInSrcDir
    dstFile = C.serverSrcDirInServerRootDir </> passportFileInSrcDir
    tmplData =
      object
        [ "providers"
            .= [ buildProviderData "google" (App.Dependency.name googlePassportDependency) (AS.Auth.isGoogleAuthEnabled auth),
                 buildProviderData "github" (App.Dependency.name gitHubPassportDependency) (AS.Auth.isGitHubAuthEnabled auth)
               ]
        ]

    buildProviderData :: String -> String -> Bool -> Aeson.Value
    buildProviderData name npmPackage isEnabled =
      object
        [ "name" .= name,
          "npmPackage" .= npmPackage,
          "isEnabled" .= isEnabled
        ]

    passportFileInSrcDir :: Path' (Rel C.ServerSrcDir) File'
    passportFileInSrcDir = [relfile|routes/auth/passport/passport.js|]

genGoogleAuth :: AS.Auth.Auth -> Generator [FileDraft]
genGoogleAuth auth
  | AS.Auth.isGoogleAuthEnabled auth =
      sequence
        [ return $ C.mkSrcTmplFd [relfile|routes/auth/passport/google/google.js|],
          return $ C.mkSrcTmplFd [relfile|routes/auth/passport/google/defaults.js|],
          return $
            mkAuthConfigFd
              [relfile|routes/auth/passport/generic/config.js|]
              [relfile|routes/auth/passport/google/config.js|]
              (Just configTmplData)
        ]
  | otherwise = return []
  where
    configTmplData = getTmplDataForAuthMethodConfig auth AS.Auth.google

genGitHubAuth :: AS.Auth.Auth -> Generator [FileDraft]
genGitHubAuth auth
  | AS.Auth.isGitHubAuthEnabled auth =
      sequence
        [ return $ C.mkSrcTmplFd [relfile|routes/auth/passport/github/github.js|],
          return $ C.mkSrcTmplFd [relfile|routes/auth/passport/github/defaults.js|],
          return $
            mkAuthConfigFd
              [relfile|routes/auth/passport/generic/config.js|]
              [relfile|routes/auth/passport/github/config.js|]
              (Just configTmplData)
        ]
  | otherwise = return []
  where
    configTmplData = getTmplDataForAuthMethodConfig auth AS.Auth.gitHub

mkAuthConfigFd ::
  Path' (Rel C.ServerTemplatesSrcDir) File' ->
  Path' (Rel C.ServerSrcDir) File' ->
  Maybe Aeson.Value ->
  FileDraft
mkAuthConfigFd pathInTemplatesSrcDir pathInGenProjectSrcDir tmplData =
  C.mkTmplFdWithDstAndData srcPath dstPath tmplData
  where
    srcPath = C.srcDirInServerTemplatesDir </> pathInTemplatesSrcDir
    dstPath = C.serverSrcDirInServerRootDir </> pathInGenProjectSrcDir

getTmplDataForAuthMethodConfig :: AS.Auth.Auth -> (AS.Auth.AuthMethods -> Maybe AS.Auth.ExternalAuthConfig) -> Aeson.Value
getTmplDataForAuthMethodConfig auth authMethod =
  object
    [ "doesConfigFnExist" .= isJust maybeConfigFn,
      "configFnImportStatement" .= fromMaybe "" maybeConfigFnImportStmt,
      "configFnIdentifier" .= fromMaybe "" maybeConfigFnImportIdentifier,
      "doesGetUserFieldsFnExist" .= isJust maybeGetUserFieldsFn,
      "getUserFieldsFnImportStatement" .= fromMaybe "" maybeOnSignInFnImportStmt,
      "getUserFieldsFnIdentifier" .= fromMaybe "" maybeOnSignInFnImportIdentifier
    ]
  where
    maybeConfigFn = AS.Auth.configFn =<< authMethod (AS.Auth.methods auth)
    maybeConfigFnImportDetails = getJsImportDetailsForExtFnImport relPosixPathFromAuthMethodDirToExtSrcDir <$> maybeConfigFn
    (maybeConfigFnImportIdentifier, maybeConfigFnImportStmt) = (fst <$> maybeConfigFnImportDetails, snd <$> maybeConfigFnImportDetails)

    maybeGetUserFieldsFn = AS.Auth.getUserFieldsFn =<< authMethod (AS.Auth.methods auth)
    maybeOnSignInFnImportDetails = getJsImportDetailsForExtFnImport relPosixPathFromAuthMethodDirToExtSrcDir <$> maybeGetUserFieldsFn
    (maybeOnSignInFnImportIdentifier, maybeOnSignInFnImportStmt) = (fst <$> maybeOnSignInFnImportDetails, snd <$> maybeOnSignInFnImportDetails)

    relPosixPathFromAuthMethodDirToExtSrcDir :: Path Posix (Rel (Dir C.ServerSrcDir)) (Dir GeneratedExternalCodeDir)
    relPosixPathFromAuthMethodDirToExtSrcDir = [reldirP|../../../../|] </> fromJust (SP.relDirToPosix extServerCodeDirInServerSrcDir)

depsRequiredByPassport :: AppSpec -> [App.Dependency.Dependency]
depsRequiredByPassport spec =
  concat
    [ [App.Dependency.make ("passport", "0.6.0") | (AS.App.Auth.isExternalAuthEnabled <$> maybeAuth) == Just True],
      [googlePassportDependency | (AS.App.Auth.isGoogleAuthEnabled <$> maybeAuth) == Just True],
      [gitHubPassportDependency | (AS.App.Auth.isGitHubAuthEnabled <$> maybeAuth) == Just True]
    ]
  where
    maybeAuth = AS.App.auth $ snd $ getApp spec

googlePassportDependency :: App.Dependency.Dependency
googlePassportDependency = App.Dependency.make ("passport-google-oauth20", "2.0.0")

gitHubPassportDependency :: App.Dependency.Dependency
gitHubPassportDependency = App.Dependency.make ("passport-github2", "0.1.12")
