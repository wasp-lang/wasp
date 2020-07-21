module Generator.WebAppGenerator.Common
    ( webAppRootDirInProjectRootDir
    , webAppSrcDirInWebAppRootDir
    , copyTmplAsIs
    , makeSimpleTemplateFD
    , makeTemplateFD
    , webAppSrcDirInProjectRootDir
    , webAppTemplatesDirInTemplatesDir
    ) where

import qualified Data.Aeson as Aeson
import Path ((</>), reldir)
import qualified Path.Aliases as Path
import Wasp (Wasp)
import Generator.FileDraft (FileDraft, createTemplateFileDraft)


-- * Paths

-- | Path where web app root dir is generated, relative to the root directory of the whole generated project.
webAppRootDirInProjectRootDir :: Path.RelDir
webAppRootDirInProjectRootDir = [reldir|web-app|]

-- | Path to generated web app src/ directory, relative to the root directory of generated web app.
webAppSrcDirInWebAppRootDir :: Path.RelDir
webAppSrcDirInWebAppRootDir = [reldir|src|]

webAppSrcDirInProjectRootDir :: Path.RelDir
webAppSrcDirInProjectRootDir = webAppRootDirInProjectRootDir </> webAppSrcDirInWebAppRootDir

-- * Templates

copyTmplAsIs :: Path.RelFile -> FileDraft
copyTmplAsIs path = makeTemplateFD path path Nothing

makeSimpleTemplateFD :: Path.RelFile -> Wasp -> FileDraft
makeSimpleTemplateFD path wasp = makeTemplateFD path path (Just $ Aeson.toJSON wasp)

makeTemplateFD :: Path.RelFile -> Path.RelFile -> Maybe Aeson.Value -> FileDraft
makeTemplateFD srcPathInWebAppTemplatesDir dstPathInWebAppRootDir tmplData =
    createTemplateFileDraft
        (webAppRootDirInProjectRootDir </> dstPathInWebAppRootDir)
        (webAppTemplatesDirInTemplatesDir </> srcPathInWebAppTemplatesDir)
        tmplData

-- | Path in templates directory where web app templates reside.
webAppTemplatesDirInTemplatesDir :: Path.RelDir
webAppTemplatesDirInTemplatesDir = [reldir|react-app|]
