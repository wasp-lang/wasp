module Generator.ServerGenerator.Common
    ( serverRootDirInProjectRootDir
    , serverSrcDirInServerRootDir
    , serverSrcDirInProjectRootDir
    , copyTmplAsIs
    , makeSimpleTemplateFD
    , makeTemplateFD
    ) where

import qualified Data.Aeson as Aeson
import Path ((</>), reldir)
import qualified Path.Aliases as Path
import Wasp (Wasp)
import Generator.FileDraft (FileDraft, createTemplateFileDraft)


-- * Paths

-- | Path where server root dir is generated, relative to the root directory of the whole generated project.
serverRootDirInProjectRootDir :: Path.RelDir
serverRootDirInProjectRootDir = [reldir|server|]

-- | Path to generated server src/ directory, relative to the root directory of generated server.
serverSrcDirInServerRootDir :: Path.RelDir
serverSrcDirInServerRootDir = [reldir|src|]

serverSrcDirInProjectRootDir :: Path.RelDir
serverSrcDirInProjectRootDir = serverRootDirInProjectRootDir </> serverSrcDirInServerRootDir

-- * Templates

copyTmplAsIs :: Path.RelFile -> FileDraft
copyTmplAsIs path = makeTemplateFD path path Nothing

makeSimpleTemplateFD :: Path.RelFile -> Wasp -> FileDraft
makeSimpleTemplateFD path wasp = makeTemplateFD path path (Just $ Aeson.toJSON wasp)

makeTemplateFD :: Path.RelFile -> Path.RelFile -> Maybe Aeson.Value -> FileDraft
makeTemplateFD relSrcPath relDstPath tmplData =
    createTemplateFileDraft
        (serverRootDirInProjectRootDir </> relDstPath)
        (serverTemplatesDirInTemplatesDir </> relSrcPath)
        tmplData

-- | Path in templates directory where server app templates reside.
serverTemplatesDirInTemplatesDir :: Path.RelDir
serverTemplatesDirInTemplatesDir = [reldir|server|]
