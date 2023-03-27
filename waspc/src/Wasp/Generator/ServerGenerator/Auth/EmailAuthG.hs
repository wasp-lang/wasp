module Wasp.Generator.ServerGenerator.Auth.EmailAuthG
  ( genEmailAuth,
  )
where

import Data.Aeson (object, (.=))
import Data.Maybe (fromMaybe)
import StrongPath
  ( File',
    Path',
    Rel,
    relfile,
    (</>),
  )
import qualified StrongPath as SP
import qualified Wasp.AppSpec as AS
import qualified Wasp.AppSpec.App.Auth as AS.Auth
import qualified Wasp.AppSpec.App.EmailSender as AS.EmailSender
import Wasp.AppSpec.Util (findRoutePathFromRef)
import Wasp.Generator.AuthProviders (emailAuthProvider)
import qualified Wasp.Generator.AuthProviders.Email as Email
import Wasp.Generator.FileDraft (FileDraft)
import Wasp.Generator.Monad (Generator)
import qualified Wasp.Generator.ServerGenerator.Common as C

genEmailAuth :: AS.AppSpec -> AS.Auth.Auth -> Generator [FileDraft]
genEmailAuth spec auth = case emailAuth of
  Just emailAuthConfig ->
    sequence
      [ genEmailAuthConfig spec emailAuthConfig,
        genTypes emailAuthConfig
      ]
  _ -> return []
  where
    emailAuth = AS.Auth.email $ AS.Auth.methods auth

genEmailAuthConfig :: AS.AppSpec -> AS.Auth.EmailAuthConfig -> Generator FileDraft
genEmailAuthConfig spec emailAuthConfig = return $ C.mkTmplFdWithDstAndData tmplFile dstFile (Just tmplData)
  where
    tmplFile = C.srcDirInServerTemplatesDir </> SP.castRel authIndexFileInSrcDir
    dstFile = C.serverSrcDirInServerRootDir </> authIndexFileInSrcDir

    tmplData =
      object
        [ "providerId" .= Email.providerId emailAuthProvider,
          "displayName" .= Email.displayName emailAuthProvider,
          "fromField" .= fromFieldJson,
          "onVerifySuccessRedirectTo" .= onVerifySuccessRedirectTo,
          "passwordResetClientRoute" .= passwordResetClientRoute
        ]

    fromFieldJson =
      object
        [ "name" .= fromMaybe "" maybeName,
          "email" .= email
        ]

    fromField = AS.Auth.fromField emailAuthConfig
    maybeName = AS.EmailSender.name fromField
    email = AS.EmailSender.email fromField

    maybeOnVerifySuccessRoute = findRoutePathFromRef spec $ AS.Auth.onVerifySuccessRedirectTo . AS.Auth.emailVerfication $ emailAuthConfig
    onVerifySuccessRedirectTo = fromMaybe "/" maybeOnVerifySuccessRoute

    maybePasswordResetClientRoute = findRoutePathFromRef spec $ AS.Auth.clientRoute . AS.Auth.passwordReset $ emailAuthConfig
    passwordResetClientRoute = fromMaybe "/" maybePasswordResetClientRoute

    authIndexFileInSrcDir :: Path' (Rel C.ServerSrcDir) File'
    authIndexFileInSrcDir = [relfile|auth/providers/config/email.ts|]

genTypes :: AS.Auth.EmailAuthConfig -> Generator FileDraft
genTypes _emailAuthConfig = return $ C.mkTmplFdWithData tmplFile (Just tmplData)
  where
    tmplFile = C.srcDirInServerTemplatesDir </> [relfile|auth/providers/email/types.ts|]
    tmplData = object []
