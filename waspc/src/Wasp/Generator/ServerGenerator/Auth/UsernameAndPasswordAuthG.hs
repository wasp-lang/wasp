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
    reldirP,
    relfile,
    (</>),
  )
import qualified StrongPath as SP
import qualified Wasp.AppSpec.App.Auth as AS.Auth
import Wasp.Generator.AuthProviders (usernameAndPasswordAuthProvider)
import qualified Wasp.Generator.AuthProviders.UsernameAndPassword as UsernameAndPassword
import Wasp.Generator.FileDraft (FileDraft)
import Wasp.Generator.Monad (Generator)
import qualified Wasp.Generator.ServerGenerator.Common as C
import Wasp.Generator.ServerGenerator.JsImport (extImportToImportJson)

genUsernameAndPasswordAuth :: AS.Auth.Auth -> Generator [FileDraft]
genUsernameAndPasswordAuth auth = case usernameAndPasswordAuth of
  Just usernameAndPasswordAuthConfig ->
    sequence [genUsernameAndPasswordAuthConfig usernameAndPasswordAuthConfig]
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
