module Wasp.Generator.ServerGenerator.EmailG where

import Data.Aeson (object, (.=))
import qualified Data.Aeson as Aeson
import Data.Maybe (isJust)
import StrongPath (File', Path', Rel, relfile, (</>))
import Wasp.AppSpec (AppSpec)
import qualified Wasp.AppSpec.App as AS.App
import qualified Wasp.AppSpec.App.Dependency as AS.Dependency
import Wasp.AppSpec.App.Email (Email)
import qualified Wasp.AppSpec.App.Email as AS.Email
import Wasp.AppSpec.Valid (getApp)
import Wasp.Generator.FileDraft (FileDraft)
import Wasp.Generator.Monad (Generator)
import qualified Wasp.Generator.ServerGenerator.Common as C
import qualified Wasp.SemanticVersion as SV
import Wasp.Util ((<++>))

genEmail :: AppSpec -> Generator [FileDraft]
genEmail spec = case maybeEmail of
  Just email ->
    sequence
      [ genIndex email
      ]
      <++> genCore email
  Nothing -> return []
  where
    maybeEmail = AS.App.email $ snd $ getApp spec

genIndex :: Email -> Generator FileDraft
genIndex email = return $ C.mkTmplFdWithDstAndData srcPath dstPath (Just tmplData)
  where
    srcPath = C.srcDirInServerTemplatesDir </> [relfile|email/index.ts|]
    dstPath = C.serverSrcDirInServerRootDir </> [relfile|email/index.ts|]
    tmplData = getEmailProvidersJson email

genCore :: Email -> Generator [FileDraft]
genCore email =
  sequence
    [ genCoreIndex email,
      copyTmplFile [relfile|email/core/types.ts|],
      genCoreHelpers email
    ]
    <++> genSmtp email
    <++> genSendGrid email
    <++> genMailgun email

genCoreIndex :: Email -> Generator FileDraft
genCoreIndex email = return $ C.mkTmplFdWithDstAndData srcPath dstPath (Just tmplData)
  where
    srcPath = C.srcDirInServerTemplatesDir </> [relfile|email/core/index.ts|]
    dstPath = C.serverSrcDirInServerRootDir </> [relfile|email/core/index.ts|]
    tmplData = getEmailProvidersJson email

genCoreHelpers :: Email -> Generator FileDraft
genCoreHelpers email = return $ C.mkTmplFdWithDstAndData srcPath dstPath (Just tmplData)
  where
    srcPath = C.srcDirInServerTemplatesDir </> [relfile|email/core/helpers.ts|]
    dstPath = C.serverSrcDirInServerRootDir </> [relfile|email/core/helpers.ts|]
    tmplData =
      object
        [ "senderDefaults"
            .= object
              [ "email" .= AS.Email.email sender,
                "title" .= title,
                "isTitleDefined" .= isJust title
              ]
        ]
    sender = AS.Email.sender email
    title = AS.Email.title sender

genSmtp :: Email -> Generator [FileDraft]
genSmtp email =
  case AS.Email.provider email of
    AS.Email.SMTP ->
      sequence
        [ copyTmplFile [relfile|email/core/providers/smtp.ts|]
        ]
    _ -> return []

genSendGrid :: Email -> Generator [FileDraft]
genSendGrid email =
  case AS.Email.provider email of
    AS.Email.SendGrid ->
      sequence
        [ copyTmplFile [relfile|email/core/providers/sendgrid.ts|]
        ]
    _ -> return []

genMailgun :: Email -> Generator [FileDraft]
genMailgun email =
  case AS.Email.provider email of
    AS.Email.Mailgun ->
      sequence
        [ copyTmplFile [relfile|email/core/providers/mailgun.ts|]
        ]
    _ -> return []

getEmailProvidersJson :: Email -> Aeson.Value
getEmailProvidersJson email =
  object
    [ "isSmtpProviderUsed" .= isSmtpProviderUsed,
      "isSendGridProviderUsed" .= isSendGridProviderUsed,
      "isMailgunProviderUsed" .= isMailgunProviderUsed
    ]
  where
    isSmtpProviderUsed = provider == AS.Email.SMTP
    isSendGridProviderUsed = provider == AS.Email.SendGrid
    isMailgunProviderUsed = provider == AS.Email.Mailgun
    provider = AS.Email.provider email

copyTmplFile :: Path' (Rel C.ServerTemplatesSrcDir) File' -> Generator FileDraft
copyTmplFile = return . C.mkSrcTmplFd

-- Dependencies

nodeMailerVersionRange :: SV.Range
nodeMailerVersionRange = SV.Range [SV.backwardsCompatibleWith (SV.Version 6 9 1)]

nodeMailerDependency :: AS.Dependency.Dependency
nodeMailerDependency = AS.Dependency.make ("nodemailer", show nodeMailerVersionRange)

sendGridVersionRange :: SV.Range
sendGridVersionRange = SV.Range [SV.backwardsCompatibleWith (SV.Version 7 7 0)]

sendGridDependency :: AS.Dependency.Dependency
sendGridDependency = AS.Dependency.make ("@sendgrid/mail", show sendGridVersionRange)

mailgunVersionRange :: SV.Range
mailgunVersionRange = SV.Range [SV.backwardsCompatibleWith (SV.Version 0 5 1)]

mailgunDependency :: AS.Dependency.Dependency
mailgunDependency = AS.Dependency.make ("ts-mailgun", show mailgunVersionRange)

depsRequiredByEmail :: AppSpec -> [AS.Dependency.Dependency]
depsRequiredByEmail spec =
  concat
    [ [nodeMailerDependency | provider == Just AS.Email.SMTP],
      [sendGridDependency | provider == Just AS.Email.SendGrid],
      [mailgunDependency | provider == Just AS.Email.Mailgun]
    ]
  where
    provider = AS.Email.provider <$> (AS.App.email . snd . getApp $ spec)
