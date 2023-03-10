module Wasp.Generator.ServerGenerator.EmailG where

import Data.Aeson (object, (.=))
import Data.Maybe (isJust)
import StrongPath (File', Path', Rel, reldir, relfile, (</>))
import qualified StrongPath as SP
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
    tmplData = object ["emailProvider" .= show provider]
    provider = AS.Email.provider email

genCore :: Email -> Generator [FileDraft]
genCore email =
  sequence
    [ copyTmplFile [relfile|email/core/index.ts|],
      copyTmplFile [relfile|email/core/helpers.ts|],
      copyTmplFile [relfile|email/core/types.ts|]
    ]
    <++> genSmtp email

genSmtp :: Email -> Generator [FileDraft]
genSmtp email =
  case AS.Email.provider email of
    AS.Email.SMTP ->
      sequence
        [ copyTmplFile [relfile|email/core/providers/smtp.ts|]
        ]
    _ -> return []

copyTmplFile :: Path' (Rel C.ServerTemplatesSrcDir) File' -> Generator FileDraft
copyTmplFile = return . C.mkSrcTmplFd

nodeMailerVesionRange :: SV.Range
nodeMailerVesionRange = SV.Range [SV.backwardsCompatibleWith (SV.Version 6 9 1)]

nodeMailerDependency :: AS.Dependency.Dependency
nodeMailerDependency = AS.Dependency.make ("nodemailer", show nodeMailerVesionRange)

sendGridVesionRange :: SV.Range
sendGridVesionRange = SV.Range [SV.backwardsCompatibleWith (SV.Version 7 7 0)]

sendGridDependency :: AS.Dependency.Dependency
sendGridDependency = AS.Dependency.make ("@sendgrid/mail", show sendGridVesionRange)

depsRequiredByEmail :: AppSpec -> [AS.Dependency.Dependency]
depsRequiredByEmail spec =
  [nodeMailerDependency | isSmtpUsed]
    ++ [sendGridDependency | isSendGridUser]
  where
    isSmtpUsed = Just AS.Email.SMTP == provider
    isSendGridUser = Just AS.Email.SendGrid == provider
    provider = AS.Email.provider <$> (AS.App.email . snd . getApp $ spec)
