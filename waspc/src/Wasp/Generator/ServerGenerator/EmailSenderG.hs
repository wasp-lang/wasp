module Wasp.Generator.ServerGenerator.EmailSenderG where

import Data.Aeson (object, (.=))
import qualified Data.Aeson as Aeson
import Data.Maybe (isJust)
import StrongPath (File', Path', Rel, relfile, (</>))
import Wasp.AppSpec (AppSpec)
import qualified Wasp.AppSpec.App as AS.App
import qualified Wasp.AppSpec.App.Dependency as AS.Dependency
import Wasp.AppSpec.App.EmailSender (EmailSender)
import qualified Wasp.AppSpec.App.EmailSender as AS.EmailSender
import Wasp.AppSpec.Valid (getApp)
import Wasp.Generator.FileDraft (FileDraft)
import Wasp.Generator.Monad (Generator)
import qualified Wasp.Generator.ServerGenerator.Common as C
import qualified Wasp.SemanticVersion as SV
import Wasp.Util ((<++>))

genEmailSender :: AppSpec -> Generator [FileDraft]
genEmailSender spec = case maybeEmailSender of
  Just emailSender ->
    sequence
      [ genIndex emailSender
      ]
      <++> genCore emailSender
  Nothing -> return []
  where
    maybeEmailSender = AS.App.emailSender $ snd $ getApp spec

genIndex :: EmailSender -> Generator FileDraft
genIndex email = return $ C.mkTmplFdWithDstAndData srcPath dstPath (Just tmplData)
  where
    srcPath = C.srcDirInServerTemplatesDir </> [relfile|email/index.ts|]
    dstPath = C.serverSrcDirInServerRootDir </> [relfile|email/index.ts|]
    tmplData = getEmailProvidersJson email

genCore :: EmailSender -> Generator [FileDraft]
genCore email =
  sequence
    [ genCoreIndex email,
      copyTmplFile [relfile|email/core/types.ts|],
      genCoreHelpers email,
      copyTmplFile [relfile|email/core/providers/dummy.ts|]
    ]
    <++> genSmtp email
    <++> genSendGrid email
    <++> genMailgun email

genCoreIndex :: EmailSender -> Generator FileDraft
genCoreIndex email = return $ C.mkTmplFdWithDstAndData srcPath dstPath (Just tmplData)
  where
    srcPath = C.srcDirInServerTemplatesDir </> [relfile|email/core/index.ts|]
    dstPath = C.serverSrcDirInServerRootDir </> [relfile|email/core/index.ts|]
    tmplData = getEmailProvidersJson email

genCoreHelpers :: EmailSender -> Generator FileDraft
genCoreHelpers email = return $ C.mkTmplFdWithDstAndData srcPath dstPath (Just tmplData)
  where
    srcPath = C.srcDirInServerTemplatesDir </> [relfile|email/core/helpers.ts|]
    dstPath = C.serverSrcDirInServerRootDir </> [relfile|email/core/helpers.ts|]
    tmplData =
      object
        [ "senderDefaults"
            .= object
              [ "email" .= AS.EmailSender.email emailFrom,
                "title" .= title,
                "isTitleDefined" .= isJust title
              ]
        ]
    emailFrom = AS.EmailSender.defaultFrom email
    title = AS.EmailSender.title emailFrom

genSmtp :: EmailSender -> Generator [FileDraft]
genSmtp email =
  case AS.EmailSender.provider email of
    AS.EmailSender.SMTP ->
      sequence
        [ copyTmplFile [relfile|email/core/providers/smtp.ts|]
        ]
    _ -> return []

genSendGrid :: EmailSender -> Generator [FileDraft]
genSendGrid email =
  case AS.EmailSender.provider email of
    AS.EmailSender.SendGrid ->
      sequence
        [ copyTmplFile [relfile|email/core/providers/sendgrid.ts|]
        ]
    _ -> return []

genMailgun :: EmailSender -> Generator [FileDraft]
genMailgun email =
  case AS.EmailSender.provider email of
    AS.EmailSender.Mailgun ->
      sequence
        [ copyTmplFile [relfile|email/core/providers/mailgun.ts|]
        ]
    _ -> return []

getEmailProvidersJson :: EmailSender -> Aeson.Value
getEmailProvidersJson email =
  object
    [ "isSmtpProviderUsed" .= isSmtpProviderUsed,
      "isSendGridProviderUsed" .= isSendGridProviderUsed,
      "isMailgunProviderUsed" .= isMailgunProviderUsed
    ]
  where
    isSmtpProviderUsed = provider == AS.EmailSender.SMTP
    isSendGridProviderUsed = provider == AS.EmailSender.SendGrid
    isMailgunProviderUsed = provider == AS.EmailSender.Mailgun
    provider = AS.EmailSender.provider email

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
    [ [nodeMailerDependency | provider == Just AS.EmailSender.SMTP],
      [sendGridDependency | provider == Just AS.EmailSender.SendGrid],
      [mailgunDependency | provider == Just AS.EmailSender.Mailgun]
    ]
  where
    provider = AS.EmailSender.provider <$> (AS.App.emailSender . snd . getApp $ spec)
