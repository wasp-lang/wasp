module Wasp.Generator.ServerGenerator.Auth.UsernameAndPasswordAuthG
  ( genUsernameAndPasswordAuth,
  )
where

import Data.Aeson (object, (.=))
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
import qualified Wasp.AppSpec as AS
import qualified Wasp.AppSpec.App.Auth as AS.Auth
import Wasp.Generator.AuthProviders (usernameAndPasswordAuthProvider)
import qualified Wasp.Generator.AuthProviders.UsernameAndPassword as UsernameAndPassword
import Wasp.Generator.FileDraft (FileDraft)
import Wasp.Generator.Monad (Generator)
import qualified Wasp.Generator.ServerGenerator.Common as C
import Wasp.Generator.ServerGenerator.JsImport (extImportToImportJson)
import qualified Wasp.Util as Util

genUsernameAndPasswordAuth :: AS.Auth.Auth -> Generator [FileDraft]
genUsernameAndPasswordAuth auth = case usernameAndPasswordAuth of
  Just usernameAndPasswordAuthConfig ->
    sequence
      [ genUsernameAndPasswordAuthConfig usernameAndPasswordAuthConfig,
        genLoginRoute auth,
        genSignupRoute auth
      ]
  Nothing -> return []
  where
    usernameAndPasswordAuth = AS.Auth.usernameAndPassword $ AS.Auth.methods auth

genUsernameAndPasswordAuthConfig :: AS.Auth.UsernameAndPasswordConfig -> Generator FileDraft
genUsernameAndPasswordAuthConfig usernameAndPasswordConfig = return $ C.mkTmplFdWithDstAndData tmplFile dstFile (Just tmplData)
  where
    tmplFile = C.srcDirInServerTemplatesDir </> SP.castRel authIndexFileInSrcDir
    dstFile = C.serverSrcDirInServerRootDir </> authIndexFileInSrcDir

    tmplData =
      object
        [ "providerId" .= UsernameAndPassword.providerId usernameAndPasswordAuthProvider,
          "displayName" .= UsernameAndPassword.displayName usernameAndPasswordAuthProvider,
          "userSignupFields" .= extImportToImportJson relPathToServerSrcDir maybeUserSignupFields
        ]

    maybeUserSignupFields = AS.Auth.userSignupFieldsForUsernameAuth usernameAndPasswordConfig

    authIndexFileInSrcDir :: Path' (Rel C.ServerSrcDir) File'
    authIndexFileInSrcDir = [relfile|auth/providers/config/username.ts|]

    relPathToServerSrcDir :: Path Posix (Rel importLocation) (Dir C.ServerSrcDir)
    relPathToServerSrcDir = [reldirP|../../../|]

genLoginRoute :: AS.Auth.Auth -> Generator FileDraft
genLoginRoute auth = return $ C.mkTmplFdWithDstAndData tmplFile dstFile (Just tmplData)
  where
    loginRouteRelToSrc = [relfile|auth/providers/username/login.ts|]
    tmplFile = C.asTmplFile $ [reldir|src|] </> loginRouteRelToSrc
    dstFile = C.serverSrcDirInServerRootDir </> C.asServerSrcFile loginRouteRelToSrc

    tmplData =
      let userEntityName = AS.refName $ AS.Auth.userEntity auth
       in object
            [ "userEntityUpper" .= (userEntityName :: String),
              "userEntityLower" .= (Util.toLowerFirst userEntityName :: String)
            ]

genSignupRoute :: AS.Auth.Auth -> Generator FileDraft
genSignupRoute auth = return $ C.mkTmplFdWithDstAndData tmplFile dstFile (Just tmplData)
  where
    signupRouteRelToSrc = [relfile|auth/providers/username/signup.ts|]
    tmplFile = C.asTmplFile $ [reldir|src|] </> signupRouteRelToSrc
    dstFile = C.serverSrcDirInServerRootDir </> C.asServerSrcFile signupRouteRelToSrc

    tmplData =
      object
        [ "userEntityLower" .= (Util.toLowerFirst (AS.refName $ AS.Auth.userEntity auth) :: String)
        ]
