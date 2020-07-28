module Generator.ServerGenerator.Common
    ( serverRootDirInProjectRootDir
    , serverSrcDirInServerRootDir
    , serverSrcDirInProjectRootDir
    , copyTmplAsIs
    , makeSimpleTemplateFD
    , makeTemplateFD
    , copySrcTmplAsIs
    , srcDirInServerTemplatesDir
    , asTmplFile
    , asServerFile
    , ServerRootDir
    , ServerSrcDir
    , ServerTemplatesDir
    , ServerTemplatesSrcDir
    ) where

import qualified Data.Aeson as Aeson
import qualified Path as P

import StrongPath (Path, Rel, File, Dir, (</>))
import qualified StrongPath as SP
import Wasp (Wasp)
import Generator.FileDraft (FileDraft, createTemplateFileDraft)
import Generator.Common (ProjectRootDir)
import Generator.Templates (TemplatesDir)


data ServerRootDir
data ServerSrcDir
data ServerTemplatesDir
data ServerTemplatesSrcDir


asTmplFile :: P.Path P.Rel P.File -> Path (Rel ServerTemplatesDir) File
asTmplFile = SP.fromPathRelFile

asServerFile :: P.Path P.Rel P.File -> Path (Rel ServerRootDir) File
asServerFile = SP.fromPathRelFile


-- * Paths

-- | Path where server root dir is generated.
serverRootDirInProjectRootDir :: Path (Rel ProjectRootDir) (Dir ServerRootDir)
serverRootDirInProjectRootDir = SP.fromPathRelDir [P.reldir|server|]

-- | Path to generated server src/ directory.
serverSrcDirInServerRootDir :: Path (Rel ServerRootDir) (Dir ServerSrcDir)
serverSrcDirInServerRootDir = SP.fromPathRelDir [P.reldir|src|]

serverSrcDirInProjectRootDir :: Path (Rel ProjectRootDir) (Dir ServerSrcDir)
serverSrcDirInProjectRootDir = serverRootDirInProjectRootDir </> serverSrcDirInServerRootDir


-- * Templates

copyTmplAsIs :: Path (Rel ServerTemplatesDir) File -> FileDraft
copyTmplAsIs srcPath = makeTemplateFD srcPath dstPath Nothing
    where dstPath = (SP.castRel srcPath) :: Path (Rel ServerRootDir) File

makeSimpleTemplateFD :: Path (Rel ServerTemplatesDir) File -> Wasp -> FileDraft
makeSimpleTemplateFD srcPath wasp = makeTemplateFD srcPath dstPath (Just $ Aeson.toJSON wasp)
    where dstPath = (SP.castRel srcPath) :: Path (Rel ServerRootDir) File

makeTemplateFD :: Path (Rel ServerTemplatesDir) File
               -> Path (Rel ServerRootDir) File
               -> Maybe Aeson.Value
               -> FileDraft
makeTemplateFD relSrcPath relDstPath tmplData =
    createTemplateFileDraft
        (serverRootDirInProjectRootDir </> relDstPath)
        (serverTemplatesDirInTemplatesDir </> relSrcPath)
        tmplData

copySrcTmplAsIs :: Path (Rel ServerTemplatesSrcDir) File -> FileDraft
copySrcTmplAsIs pathInTemplatesSrcDir = makeTemplateFD srcPath dstPath Nothing
  where srcPath = srcDirInServerTemplatesDir </> pathInTemplatesSrcDir
        dstPath = serverSrcDirInServerRootDir
                  </> ((SP.castRel pathInTemplatesSrcDir) :: Path (Rel ServerSrcDir) File)

-- | Path where server app templates reside.
serverTemplatesDirInTemplatesDir :: Path (Rel TemplatesDir) (Dir ServerTemplatesDir)
serverTemplatesDirInTemplatesDir = SP.fromPathRelDir [P.reldir|server|]

srcDirInServerTemplatesDir :: Path (Rel ServerTemplatesDir) (Dir ServerTemplatesSrcDir)
srcDirInServerTemplatesDir = SP.fromPathRelDir [P.reldir|src|]
