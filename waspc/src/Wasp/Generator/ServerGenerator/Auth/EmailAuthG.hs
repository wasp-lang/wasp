module Wasp.Generator.ServerGenerator.Auth.EmailAuthG
  ( genEmailAuth,
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
    reldirP,
    relfile,
    (</>),
  )
import qualified StrongPath as SP
import qualified Wasp.AppSpec as AS
import qualified Wasp.AppSpec.App.Auth as AS.Auth
import qualified Wasp.AppSpec.App.Auth.EmailVerification as AS.Auth.EmailVerification
import qualified Wasp.AppSpec.App.Auth.PasswordReset as AS.Auth.PasswordReset
import qualified Wasp.AppSpec.App.EmailSender as AS.EmailSender
import Wasp.AppSpec.Util (getRoutePathFromRef)
import Wasp.Generator.AuthProviders (emailAuthProvider)
import qualified Wasp.Generator.AuthProviders.Email as Email
import Wasp.Generator.FileDraft (FileDraft)
import Wasp.Generator.Monad (Generator)
import qualified Wasp.Generator.ServerGenerator.Common as C
import Wasp.Generator.ServerGenerator.JsImport (extImportToImportJson)
import Wasp.Util ((<++>))

genEmailAuth :: AS.AppSpec -> AS.Auth.Auth -> Generator [FileDraft]
genEmailAuth spec auth = case emailAuth of
  Just emailAuthConfig ->
    sequence
      [ genEmailAuthConfig spec emailAuthConfig,
        genTypes emailAuthConfig
      ]
      <++> genRoutes
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
          "emailVerificationClientRoute" .= emailVerificationClientRoute,
          "passwordResetClientRoute" .= passwordResetClientRoute,
          "getPasswordResetEmailContent" .= getPasswordResetEmailContent,
          "getVerificationEmailContent" .= getVerificationEmailContent,
          "allowUnverifiedLogin" .= fromMaybe False (AS.Auth.allowUnverifiedLogin emailAuthConfig)
        ]

    fromFieldJson =
      object
        [ "name" .= fromMaybe "" maybeName,
          "email" .= email
        ]

    fromField = AS.Auth.fromField emailAuthConfig
    maybeName = AS.EmailSender.name fromField
    email = AS.EmailSender.email fromField

    emailVerificationClientRoute = getRoutePathFromRef spec $ AS.Auth.EmailVerification.clientRoute emailVerification
    passwordResetClientRoute = getRoutePathFromRef spec $ AS.Auth.PasswordReset.clientRoute passwordReset
    getPasswordResetEmailContent = extImportToImportJson relPathToServerSrcDir $ AS.Auth.PasswordReset.getEmailContentFn passwordReset
    getVerificationEmailContent = extImportToImportJson relPathToServerSrcDir $ AS.Auth.EmailVerification.getEmailContentFn emailVerification

    emailVerification = AS.Auth.emailVerification emailAuthConfig
    passwordReset = AS.Auth.passwordReset emailAuthConfig

    relPathToServerSrcDir :: Path Posix (Rel importLocation) (Dir C.ServerSrcDir)
    relPathToServerSrcDir = [reldirP|../../../|]

    authIndexFileInSrcDir :: Path' (Rel C.ServerSrcDir) File'
    authIndexFileInSrcDir = [relfile|auth/providers/config/email.ts|]

genRoutes :: Generator [FileDraft]
genRoutes =
  sequence
    [ genFileCopy [relfile|auth/providers/email/signup.ts|],
      genFileCopy [relfile|auth/providers/email/login.ts|],
      genFileCopy [relfile|auth/providers/email/resetPassword.ts|],
      genFileCopy [relfile|auth/providers/email/requestPasswordReset.ts|],
      genFileCopy [relfile|auth/providers/email/verifyEmail.ts|]
    ]
  where
    genFileCopy = return . C.mkSrcTmplFd

genTypes :: AS.Auth.EmailAuthConfig -> Generator FileDraft
genTypes _emailAuthConfig = return $ C.mkTmplFdWithData tmplFile (Just tmplData)
  where
    tmplFile = C.srcDirInServerTemplatesDir </> [relfile|auth/providers/email/types.ts|]
    tmplData = object []
