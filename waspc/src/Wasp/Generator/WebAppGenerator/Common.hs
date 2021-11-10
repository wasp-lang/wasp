module Wasp.Generator.WebAppGenerator.Common
  ( webAppRootDirInProjectRootDir,
    webAppSrcDirInWebAppRootDir,
    copyTmplAsIs,
    makeSimpleTemplateFD,
    makeTemplateFD,
    webAppSrcDirInProjectRootDir,
    webAppTemplatesDirInTemplatesDir,
    asTmplFile,
    asWebAppFile,
    asWebAppSrcFile,
    WebAppRootDir,
    WebAppSrcDir,
    WebAppTemplatesDir,
  )
where

import qualified Data.Aeson as Aeson
import Wasp.Generator.Common (ProjectRootDir)
import Wasp.Generator.FileDraft (FileDraft, createTemplateFileDraft)
import Wasp.Generator.Templates (TemplatesDir)
import StrongPath (Dir, File', Path', Rel, reldir, (</>))
import qualified StrongPath as SP
import Wasp.Wasp (Wasp)

data WebAppRootDir

data WebAppSrcDir

data WebAppTemplatesDir

asTmplFile :: Path' (Rel d) File' -> Path' (Rel WebAppTemplatesDir) File'
asTmplFile = SP.castRel

asWebAppFile :: Path' (Rel d) File' -> Path' (Rel WebAppRootDir) File'
asWebAppFile = SP.castRel

asWebAppSrcFile :: Path' (Rel d) File' -> Path' (Rel WebAppSrcDir) File'
asWebAppSrcFile = SP.castRel

-- * Paths

-- | Path where web app root dir is generated, relative to the root directory of the whole generated project.
webAppRootDirInProjectRootDir :: Path' (Rel ProjectRootDir) (Dir WebAppRootDir)
webAppRootDirInProjectRootDir = [reldir|web-app|]

-- | Path to generated web app src/ directory, relative to the root directory of generated web app.
webAppSrcDirInWebAppRootDir :: Path' (Rel WebAppRootDir) (Dir WebAppSrcDir)
webAppSrcDirInWebAppRootDir = [reldir|src|]

webAppSrcDirInProjectRootDir :: Path' (Rel ProjectRootDir) (Dir WebAppSrcDir)
webAppSrcDirInProjectRootDir = webAppRootDirInProjectRootDir </> webAppSrcDirInWebAppRootDir

-- * Templates

-- | Path in templates directory where web app templates reside.
webAppTemplatesDirInTemplatesDir :: Path' (Rel TemplatesDir) (Dir WebAppTemplatesDir)
webAppTemplatesDirInTemplatesDir = [reldir|react-app|]

copyTmplAsIs :: Path' (Rel WebAppTemplatesDir) File' -> FileDraft
copyTmplAsIs path = makeTemplateFD path (SP.castRel path) Nothing

makeSimpleTemplateFD :: Path' (Rel WebAppTemplatesDir) File' -> Wasp -> FileDraft
makeSimpleTemplateFD path wasp = makeTemplateFD path (SP.castRel path) (Just $ Aeson.toJSON wasp)

makeTemplateFD ::
  Path' (Rel WebAppTemplatesDir) File' ->
  Path' (Rel WebAppRootDir) File' ->
  Maybe Aeson.Value ->
  FileDraft
makeTemplateFD srcPathInWebAppTemplatesDir dstPathInWebAppRootDir tmplData =
  createTemplateFileDraft
    (webAppRootDirInProjectRootDir </> dstPathInWebAppRootDir)
    (webAppTemplatesDirInTemplatesDir </> srcPathInWebAppTemplatesDir)
    tmplData
