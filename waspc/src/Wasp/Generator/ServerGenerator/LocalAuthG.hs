module Wasp.Generator.ServerGenerator.LocalAuthG
  ( genLocalAuth,
  )
where

import Data.Aeson (object, (.=))
import StrongPath
  ( File',
    Path',
    Rel,
    reldir,
    relfile,
    (</>),
  )
import qualified StrongPath as SP
import qualified Wasp.AppSpec as AS
import qualified Wasp.AppSpec.App.Auth as AS.Auth
import Wasp.Generator.AuthProviders (localAuthInfo)
import qualified Wasp.Generator.AuthProviders.Local as Local
import Wasp.Generator.FileDraft (FileDraft)
import Wasp.Generator.Monad (Generator)
import qualified Wasp.Generator.ServerGenerator.Common as C
import qualified Wasp.Util as Util

genLocalAuth :: AS.Auth.Auth -> Generator [FileDraft]
genLocalAuth auth
  | AS.Auth.isUsernameAndPasswordAuthEnabled auth =
      sequence
        [ genLoginRoute auth,
          genSignupRoute auth,
          genLocalAuthConfig
        ]
  | otherwise = return []

genLocalAuthConfig :: Generator FileDraft
genLocalAuthConfig = return $ C.mkTmplFdWithDstAndData tmplFile dstFile (Just tmplData)
  where
    tmplFile = C.srcDirInServerTemplatesDir </> SP.castRel authIndexFileInSrcDir
    dstFile = C.serverSrcDirInServerRootDir </> authIndexFileInSrcDir

    tmplData =
      object
        [ "slug" .= Local.slug localAuthInfo
        ]

    authIndexFileInSrcDir :: Path' (Rel C.ServerSrcDir) File'
    authIndexFileInSrcDir = [relfile|routes/auth/providers/config/local.ts|]

genLoginRoute :: AS.Auth.Auth -> Generator FileDraft
genLoginRoute auth = return $ C.mkTmplFdWithDstAndData tmplFile dstFile (Just tmplData)
  where
    loginRouteRelToSrc = [relfile|routes/auth/providers/local/login.ts|]
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
    signupRouteRelToSrc = [relfile|routes/auth/providers/local/signup.ts|]
    tmplFile = C.asTmplFile $ [reldir|src|] </> signupRouteRelToSrc
    dstFile = C.serverSrcDirInServerRootDir </> C.asServerSrcFile signupRouteRelToSrc

    tmplData =
      object
        [ "userEntityLower" .= (Util.toLowerFirst (AS.refName $ AS.Auth.userEntity auth) :: String)
        ]
