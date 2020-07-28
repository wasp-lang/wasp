module Generator.WebAppGenerator.Common
    ( webAppRootDirInProjectRootDir
    , webAppSrcDirInWebAppRootDir
    , copyTmplAsIs
    , makeSimpleTemplateFD
    , makeTemplateFD
    , webAppSrcDirInProjectRootDir
    , webAppTemplatesDirInTemplatesDir
    , asTmplFile
    , asWebAppFile
    , asWebAppSrcFile
    , WebAppRootDir
    , WebAppSrcDir
    , WebAppTemplatesDir
    ) where

import qualified Data.Aeson as Aeson
import qualified Path as P

import StrongPath (Path, Rel, Dir, File, (</>))
import qualified StrongPath as SP
import Wasp (Wasp)
import Generator.Common (ProjectRootDir)
import Generator.FileDraft (FileDraft, createTemplateFileDraft)
import Generator.Templates (TemplatesDir)


data WebAppRootDir
data WebAppSrcDir
data WebAppTemplatesDir


asTmplFile :: P.Path P.Rel P.File -> Path (Rel WebAppTemplatesDir) File
asTmplFile = SP.fromPathRelFile

asWebAppFile :: P.Path P.Rel P.File -> Path (Rel WebAppRootDir) File
asWebAppFile = SP.fromPathRelFile

asWebAppSrcFile :: P.Path P.Rel P.File -> Path (Rel WebAppSrcDir) File
asWebAppSrcFile = SP.fromPathRelFile


-- * Paths

-- | Path where web app root dir is generated, relative to the root directory of the whole generated project.
webAppRootDirInProjectRootDir :: Path (Rel ProjectRootDir) (Dir WebAppRootDir)
webAppRootDirInProjectRootDir = SP.fromPathRelDir [P.reldir|web-app|]

-- | Path to generated web app src/ directory, relative to the root directory of generated web app.
webAppSrcDirInWebAppRootDir :: Path (Rel WebAppRootDir) (Dir WebAppSrcDir)
webAppSrcDirInWebAppRootDir = SP.fromPathRelDir [P.reldir|src|]

webAppSrcDirInProjectRootDir :: Path (Rel ProjectRootDir) (Dir WebAppSrcDir)
webAppSrcDirInProjectRootDir = webAppRootDirInProjectRootDir </> webAppSrcDirInWebAppRootDir


-- * Templates

-- | Path in templates directory where web app templates reside.
webAppTemplatesDirInTemplatesDir :: Path (Rel TemplatesDir) (Dir WebAppTemplatesDir)
webAppTemplatesDirInTemplatesDir = SP.fromPathRelDir [P.reldir|react-app|]

copyTmplAsIs :: Path (Rel WebAppTemplatesDir) File -> FileDraft
copyTmplAsIs path = makeTemplateFD path (SP.castRel path) Nothing

makeSimpleTemplateFD :: Path (Rel WebAppTemplatesDir) File -> Wasp -> FileDraft
makeSimpleTemplateFD path wasp = makeTemplateFD path (SP.castRel path) (Just $ Aeson.toJSON wasp)

makeTemplateFD :: Path (Rel WebAppTemplatesDir) File -> Path (Rel WebAppRootDir) File -> Maybe Aeson.Value -> FileDraft
makeTemplateFD srcPathInWebAppTemplatesDir dstPathInWebAppRootDir tmplData =
    createTemplateFileDraft
        (webAppRootDirInProjectRootDir </> dstPathInWebAppRootDir)
        (webAppTemplatesDirInTemplatesDir </> srcPathInWebAppTemplatesDir)
        tmplData
