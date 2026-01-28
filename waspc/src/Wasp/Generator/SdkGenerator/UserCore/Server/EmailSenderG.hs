module Wasp.Generator.SdkGenerator.UserCore.Server.EmailSenderG
  ( genNewEmailSenderApi,
  )
where

import StrongPath (Dir', Path', Rel, reldir, relfile, (</>))
import Wasp.AppSpec (AppSpec)
import qualified Wasp.AppSpec.App as AS.App
import Wasp.AppSpec.App.EmailSender (EmailSender)
import Wasp.AppSpec.Valid (getApp)
import qualified Wasp.Generator.EmailSenders as EmailSenders
import Wasp.Generator.FileDraft (FileDraft)
import Wasp.Generator.Monad (Generator)
import Wasp.Generator.SdkGenerator.UserCore.Common
  ( SdkTemplatesUserCoreProjectDir,
    mkTmplFdWithData,
  )

genNewEmailSenderApi :: AppSpec -> Generator [FileDraft]
genNewEmailSenderApi spec = case maybeEmailSender of
  Just emailSender ->
    sequence [genIndex emailSender]
  Nothing -> return []
  where
    maybeEmailSender = AS.App.emailSender $ snd $ getApp spec

genIndex :: EmailSender -> Generator FileDraft
genIndex email =
  return $ mkTmplFdWithData (serverEmailDirInSdkTemplatesUserCoreProjectDir </> [relfile|index.ts|]) tmplData
  where
    tmplData = EmailSenders.getEnabledEmailProvidersJson email

serverEmailDirInSdkTemplatesUserCoreProjectDir :: Path' (Rel SdkTemplatesUserCoreProjectDir) Dir'
serverEmailDirInSdkTemplatesUserCoreProjectDir = [reldir|server/email|]
