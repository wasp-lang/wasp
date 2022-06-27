module Wasp.Generator.ServerGenerator.AuthG
  ( genAuth,
  )
where

import Data.Aeson (object, (.=))
import Data.Maybe (fromMaybe)
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
import qualified Wasp.AppSpec.App.Auth as AS.Auth
import Wasp.AppSpec.Valid (getApp)
import Wasp.Generator.ExternalCodeGenerator.Common (GeneratedExternalCodeDir)
import Wasp.Generator.FileDraft (FileDraft)
import Wasp.Generator.JsImport (getJsImportDetailsForExtFnImport)
import Wasp.Generator.Monad (Generator)
import qualified Wasp.Generator.ServerGenerator.Common as C
import Wasp.Util ((<++>))
import qualified Wasp.Util as Util

genAuth :: AppSpec -> Generator [FileDraft]
genAuth spec = case maybeAuth of
  Just auth ->
    sequence
      [ genCoreAuth auth,
        genAuthMiddleware auth,
        -- Auth routes
        genAuthRoutesIndex,
        genLoginRoute auth,
        genLogoutRoute,
        genSignupRoute auth,
        genMeRoute auth
      ]
      <++> (if AS.Auth.passportRequired auth then genPassportAuthMethods auth else return [])
  Nothing -> return []
  where
    maybeAuth = AS.App.auth $ snd $ getApp spec

-- | Generates core/auth file which contains auth middleware and createUser() function.
genCoreAuth :: AS.Auth.Auth -> Generator FileDraft
genCoreAuth auth = return $ C.mkTmplFdWithDstAndData tmplFile dstFile (Just tmplData)
  where
    coreAuthRelToSrc = [relfile|core/auth.js|]
    tmplFile = C.asTmplFile $ [reldir|src|] </> coreAuthRelToSrc
    dstFile = C.serverSrcDirInServerRootDir </> C.asServerSrcFile coreAuthRelToSrc

    tmplData =
      let userEntityName = AS.refName $ AS.Auth.userEntity auth
       in object
            [ "userEntityUpper" .= (userEntityName :: String),
              "userEntityLower" .= (Util.toLowerFirst userEntityName :: String)
            ]

genAuthMiddleware :: AS.Auth.Auth -> Generator FileDraft
genAuthMiddleware auth = return $ C.mkTmplFdWithDstAndData tmplFile dstFile (Just tmplData)
  where
    -- TODO(martin): In prismaMiddleware.js, we assume that 'email' and 'password' are defined in user entity.
    --   This was promised to us by AppSpec, which has validation checks for this.
    --   Names of these fields are currently hardcoded, and we are not in any way relyin on AppSpec directly here.
    --   In the future we might want to figure out a way to better encode these assumptions, either by
    --   reusing the names for 'email' and 'password' fields by importing them from AppSpec, or smth similar
    --   in that direction.
    authMiddlewareRelToSrc = [relfile|core/auth/prismaMiddleware.js|]
    tmplFile = C.asTmplFile $ [reldir|src|] </> authMiddlewareRelToSrc
    dstFile = C.serverSrcDirInServerRootDir </> C.asServerSrcFile authMiddlewareRelToSrc

    tmplData =
      let userEntityName = AS.refName $ AS.Auth.userEntity auth
       in object
            [ "userEntityUpper" .= (userEntityName :: String)
            ]

genAuthRoutesIndex :: Generator FileDraft
genAuthRoutesIndex = return $ C.mkSrcTmplFd (C.asTmplSrcFile [relfile|routes/auth/index.js|])

genLoginRoute :: AS.Auth.Auth -> Generator FileDraft
genLoginRoute auth = return $ C.mkTmplFdWithDstAndData tmplFile dstFile (Just tmplData)
  where
    loginRouteRelToSrc = [relfile|routes/auth/login.js|]
    tmplFile = C.asTmplFile $ [reldir|src|] </> loginRouteRelToSrc
    dstFile = C.serverSrcDirInServerRootDir </> C.asServerSrcFile loginRouteRelToSrc

    tmplData =
      let userEntityName = AS.refName $ AS.Auth.userEntity auth
       in object
            [ "userEntityUpper" .= (userEntityName :: String),
              "userEntityLower" .= (Util.toLowerFirst userEntityName :: String)
            ]

genLogoutRoute :: Generator FileDraft
genLogoutRoute = return $ C.mkTmplFdWithDstAndData tmplFile dstFile Nothing
  where
    logoutRouteRelToSrc = [relfile|routes/auth/logout.js|]
    tmplFile = C.asTmplFile $ [reldir|src|] </> logoutRouteRelToSrc
    dstFile = C.serverSrcDirInServerRootDir </> C.asServerSrcFile logoutRouteRelToSrc

genSignupRoute :: AS.Auth.Auth -> Generator FileDraft
genSignupRoute auth = return $ C.mkTmplFdWithDstAndData tmplFile dstFile (Just tmplData)
  where
    signupRouteRelToSrc = [relfile|routes/auth/signup.js|]
    tmplFile = C.asTmplFile $ [reldir|src|] </> signupRouteRelToSrc
    dstFile = C.serverSrcDirInServerRootDir </> C.asServerSrcFile signupRouteRelToSrc

    tmplData =
      object
        [ "userEntityLower" .= (Util.toLowerFirst (AS.refName $ AS.Auth.userEntity auth) :: String)
        ]

genMeRoute :: AS.Auth.Auth -> Generator FileDraft
genMeRoute auth = return $ C.mkTmplFdWithDstAndData tmplFile dstFile (Just tmplData)
  where
    meRouteRelToSrc = [relfile|routes/auth/me.js|]
    tmplFile = C.asTmplFile $ [reldir|src|] </> meRouteRelToSrc
    dstFile = C.serverSrcDirInServerRootDir </> C.asServerSrcFile meRouteRelToSrc

    tmplData =
      object
        [ "userEntityLower" .= (Util.toLowerFirst (AS.refName $ AS.Auth.userEntity auth) :: String)
        ]

genPassportAuthMethods :: AS.Auth.Auth -> Generator [FileDraft]
genPassportAuthMethods auth =
  genPassportJs auth
    <++> (if AS.Auth.googleAuthEnabled auth then genGoogleJs auth else return [])

genPassportJs :: AS.Auth.Auth -> Generator [FileDraft]
genPassportJs auth = return [C.mkTmplFdWithDstAndData tmplFile dstFile (Just tmplData)]
  where
    tmplFile = C.srcDirInServerTemplatesDir </> SP.castRel passportFileInSrcDir
    dstFile = C.serverSrcDirInServerRootDir </> passportFileInSrcDir
    (onSignInJsFnImportIdentifier, onSignInJsFnImportStmt) = getJsImportDetailsForExtFnImport relPosixPathFromCoreAuthDirToExtSrcDir $ AS.Auth.onSignInFn auth
    tmplData =
      object
        [ "onSignInJsFnImportStatement" .= onSignInJsFnImportStmt,
          "onSignInJsFnIdentifier" .= onSignInJsFnImportIdentifier,
          "failureRedirectPath" .= AS.Auth.onAuthFailedRedirectTo auth,
          "successRedirectPath" .= fromMaybe "/" (AS.Auth.onAuthSucceededRedirectTo auth)
        ]

    passportFileInSrcDir :: Path' (Rel C.ServerSrcDir) File'
    passportFileInSrcDir = [relfile|core/auth/passport.js|]

genGoogleJs :: AS.Auth.Auth -> Generator [FileDraft]
genGoogleJs auth = return [C.mkTmplFdWithDstAndData tmplFile dstFile (Just tmplData)]
  where
    userEntityName = AS.refName $ AS.Auth.userEntity auth
    tmplFile = C.srcDirInServerTemplatesDir </> SP.castRel googleFileInSrcDir
    dstFile = C.serverSrcDirInServerRootDir </> googleFileInSrcDir
    tmplData =
      object
        [ "configJsFnImportStatement" .= fromMaybe "" maybeConfigJsFnImportStmt,
          "configJsFnIdentifier" .= fromMaybe "" maybeConfigJsFnImportIdentifier,
          "userEntityUpper" .= (userEntityName :: String),
          "userEntityLower" .= (Util.toLowerFirst userEntityName :: String)
        ]

    googleFileInSrcDir :: Path' (Rel C.ServerSrcDir) File'
    googleFileInSrcDir = [relfile|core/auth/google.js|]

    maybeConfigJsFunction = AS.Auth.configFn <$> AS.Auth.google (AS.Auth.methods auth)
    maybeConfigJsFnImportDetails = getJsImportDetailsForExtFnImport relPosixPathFromCoreAuthDirToExtSrcDir <$> maybeConfigJsFunction
    (maybeConfigJsFnImportIdentifier, maybeConfigJsFnImportStmt) =
      (fst <$> maybeConfigJsFnImportDetails, snd <$> maybeConfigJsFnImportDetails)

-- | TODO: Make this not hardcoded!
relPosixPathFromCoreAuthDirToExtSrcDir :: Path Posix (Rel (Dir C.ServerSrcDir)) (Dir GeneratedExternalCodeDir)
relPosixPathFromCoreAuthDirToExtSrcDir = [reldirP|../../ext-src|]
