module Wasp.Generator.WebAppGenerator.Common
  ( webAppRootDirInProjectRootDir,
    webAppSrcDirInWebAppRootDir,
    dotEnvClient,
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
    WebAppRootDir,
    WebAppSrcDir,
    WebAppTemplatesDir,
    WebAppTemplatesSrcDir,
  )
where

import qualified Data.Aeson as Aeson
import StrongPath (Dir, File', Path', Rel, reldir, relfile, (</>))
import qualified StrongPath as SP
import Wasp.Common (WaspProjectDir)
import Wasp.Generator.Common (GeneratedSrcDir, ProjectRootDir, WebAppRootDir)
import Wasp.Generator.FileDraft (FileDraft, createTemplateFileDraft)
import Wasp.Generator.Templates (TemplatesDir)

data WebAppSrcDir

data WebAppTemplatesDir

data WebAppTemplatesSrcDir

instance GeneratedSrcDir WebAppSrcDir

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

-- | Path to generated web app src/ directory, relative to the root directory of the whole generated project.
webAppSrcDirInProjectRootDir :: Path' (Rel ProjectRootDir) (Dir WebAppSrcDir)
webAppSrcDirInProjectRootDir = webAppRootDirInProjectRootDir </> webAppSrcDirInWebAppRootDir

-- | Path in templates directory where web app templates reside.
webAppTemplatesDirInTemplatesDir :: Path' (Rel TemplatesDir) (Dir WebAppTemplatesDir)
webAppTemplatesDirInTemplatesDir = [reldir|react-app|]

-- | Path to the web app templates src/ directory, relative to the web app templates directory.
srcDirInWebAppTemplatesDir :: Path' (Rel WebAppTemplatesDir) (Dir WebAppTemplatesSrcDir)
srcDirInWebAppTemplatesDir = [reldir|src|]

dotEnvClient :: Path' (SP.Rel WaspProjectDir) File'
dotEnvClient = [relfile|.env.client|]

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
mkTmplFdWithDstAndData srcPathInWebAppTemplatesDir dstPathInWebAppRootDir tmplData =
  createTemplateFileDraft
    (webAppRootDirInProjectRootDir </> dstPathInWebAppRootDir)
    (webAppTemplatesDirInTemplatesDir </> srcPathInWebAppTemplatesDir)
    tmplData
