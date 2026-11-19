module Wasp.Generator.SdkGenerator.EmailSender.Providers
  ( getSetupFnFile,
    ProvidersDir,
  )
where

import StrongPath (File', Path', Rel, relfile)
import qualified Wasp.AppSpec.App.EmailSender as AS.EmailSender

data ProvidersDir

getSetupFnFile :: AS.EmailSender.EmailProvider -> Path' (Rel ProvidersDir) File'
getSetupFnFile = \case
  AS.EmailSender.SMTP -> [relfile|smtp.ts|]
  AS.EmailSender.SendGrid -> [relfile|sendgrid.ts|]
  AS.EmailSender.Mailgun -> [relfile|mailgun.ts|]
  AS.EmailSender.Resend -> [relfile|resend.ts|]
  AS.EmailSender.Dummy -> [relfile|dummy.ts|]
