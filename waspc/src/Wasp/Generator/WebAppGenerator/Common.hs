module Wasp.Generator.WebAppGenerator.Common
  ( webAppRootDirInProjectRootDir,
    webAppSrcDirInWebAppRootDir,
    mkSrcTmplFd,
    mkTmplFd,
    mkTmplFdWithDst,
    mkTmplFdWithData,
    mkTmplFdWithDstAndData,
    webAppSrcDirInProjectRootDir,
    webAppTemplatesDirInTemplatesDir,
    asTmplFile,
    asWebAppFile,
    asWebAppSrcFile,
    mkUniversalTmplFdWithDst,
    serverRootDirFromWebAppRootDir,
    WebAppRootDir,
    WebAppSrcDir,
    WebAppTemplatesDir,
    WebAppTemplatesSrcDir,
    toViteImportPath,
    staticAssetsDirInWebAppDir,
    WebAppStaticAssetsDir,
    getBaseDir,
    getDefaultClientUrl,
    defaultClientPort,
  )
where

import qualified Data.Aeson as Aeson
import Data.Maybe (fromJust, fromMaybe)
import StrongPath (Abs, Dir, File, File', Path, Path', Posix, Rel, absdirP, reldir, (</>))
import qualified StrongPath as SP
import System.FilePath (splitExtension)
import Wasp.AppSpec (AppSpec)
import qualified Wasp.AppSpec.App as AS.App
import qualified Wasp.AppSpec.App.Client as AS.App.Client
import Wasp.AppSpec.Valid (getApp)
import Wasp.Generator.Common
  ( GeneratedSrcDir,
    ProjectRootDir,
    ServerRootDir,
    UniversalTemplatesDir,
    WebAppRootDir,
    universalTemplatesDirInTemplatesDir,
  )
import Wasp.Generator.FileDraft (FileDraft, createTemplateFileDraft)
import Wasp.Generator.Templates (TemplatesDir)

data WebAppSrcDir

data WebAppTemplatesDir

data WebAppTemplatesSrcDir

data WebAppStaticAssetsDir

instance GeneratedSrcDir WebAppSrcDir

serverRootDirFromWebAppRootDir :: Path' (Rel WebAppRootDir) (Dir ServerRootDir)
serverRootDirFromWebAppRootDir = [reldir|../server|]

asTmplFile :: Path' (Rel d) File' -> Path' (Rel WebAppTemplatesDir) File'
asTmplFile = SP.castRel

asWebAppFile :: Path' (Rel d) File' -> Path' (Rel WebAppRootDir) File'
asWebAppFile = SP.castRel

asWebAppSrcFile :: Path' (Rel d) File' -> Path' (Rel WebAppSrcDir) File'
asWebAppSrcFile = SP.castRel

-- | Path where web app root dir is generated, relative to the root directory of the whole generated project.
webAppRootDirInProjectRootDir :: Path' (Rel ProjectRootDir) (Dir WebAppRootDir)
webAppRootDirInProjectRootDir = [reldir|web-app|]

-- | Path to generated web app src/ directory, relative to the root directory of generated web app.
webAppSrcDirInWebAppRootDir :: Path' (Rel WebAppRootDir) (Dir WebAppSrcDir)
webAppSrcDirInWebAppRootDir = [reldir|src|]

staticAssetsDirInWebAppDir :: Path' (Rel WebAppRootDir) (Dir WebAppStaticAssetsDir)
staticAssetsDirInWebAppDir = [reldir|public|]

-- | Path to generated web app src/ directory, relative to the root directory of the whole generated project.
webAppSrcDirInProjectRootDir :: Path' (Rel ProjectRootDir) (Dir WebAppSrcDir)
webAppSrcDirInProjectRootDir = webAppRootDirInProjectRootDir </> webAppSrcDirInWebAppRootDir

-- | Path in templates directory where web app templates reside.
webAppTemplatesDirInTemplatesDir :: Path' (Rel TemplatesDir) (Dir WebAppTemplatesDir)
webAppTemplatesDirInTemplatesDir = [reldir|react-app|]

-- | Path to the web app templates src/ directory, relative to the web app templates directory.
srcDirInWebAppTemplatesDir :: Path' (Rel WebAppTemplatesDir) (Dir WebAppTemplatesSrcDir)
srcDirInWebAppTemplatesDir = [reldir|src|]

mkSrcTmplFd :: Path' (Rel WebAppTemplatesSrcDir) File' -> FileDraft
mkSrcTmplFd pathInTemplatesSrcDir = mkTmplFdWithDst srcPath dstPath
  where
    srcPath = srcDirInWebAppTemplatesDir </> pathInTemplatesSrcDir
    dstPath =
      webAppSrcDirInWebAppRootDir
        </> (SP.castRel pathInTemplatesSrcDir :: Path' (Rel WebAppSrcDir) File')

mkTmplFd :: Path' (Rel WebAppTemplatesDir) File' -> FileDraft
mkTmplFd path = mkTmplFdWithDst path (SP.castRel path)

mkTmplFdWithDst :: Path' (Rel WebAppTemplatesDir) File' -> Path' (Rel WebAppRootDir) File' -> FileDraft
mkTmplFdWithDst src dst = mkTmplFdWithDstAndData src dst Nothing

mkTmplFdWithData :: Path' (Rel WebAppTemplatesDir) File' -> Aeson.Value -> FileDraft
mkTmplFdWithData src tmplData = mkTmplFdWithDstAndData src (SP.castRel src) (Just tmplData)

mkTmplFdWithDstAndData ::
  Path' (Rel WebAppTemplatesDir) File' ->
  Path' (Rel WebAppRootDir) File' ->
  Maybe Aeson.Value ->
  FileDraft
mkTmplFdWithDstAndData relSrcPath relDstPath tmplData =
  createTemplateFileDraft
    (webAppRootDirInProjectRootDir </> relDstPath)
    (webAppTemplatesDirInTemplatesDir </> relSrcPath)
    tmplData

mkUniversalTmplFdWithDst :: Path' (Rel UniversalTemplatesDir) File' -> Path' (Rel WebAppRootDir) File' -> FileDraft
mkUniversalTmplFdWithDst relSrcPath relDstPath =
  createTemplateFileDraft
    (webAppRootDirInProjectRootDir </> relDstPath)
    (universalTemplatesDirInTemplatesDir </> relSrcPath)
    Nothing

toViteImportPath :: Path Posix (Rel r) (File f) -> Path Posix (Rel r) (File f)
toViteImportPath = fromJust . SP.parseRelFileP . dropExtension . SP.fromRelFileP
  where
    dropExtension = fst . splitExtension

getBaseDir :: AppSpec -> Path Posix Abs (Dir ())
getBaseDir spec = fromMaybe [absdirP|/|] maybeBaseDir
  where
    maybeBaseDir = SP.parseAbsDirP =<< (AS.App.Client.baseDir =<< AS.App.client (snd $ getApp spec))

defaultClientPort :: Int
defaultClientPort = 3000

getDefaultClientUrl :: AppSpec -> String
getDefaultClientUrl spec = "http://localhost:" ++ show defaultClientPort ++ SP.fromAbsDirP (getBaseDir spec)
