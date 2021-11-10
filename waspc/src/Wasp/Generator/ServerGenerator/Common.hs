module Wasp.Generator.ServerGenerator.Common
  ( serverRootDirInProjectRootDir,
    serverSrcDirInServerRootDir,
    serverSrcDirInProjectRootDir,
    copyTmplAsIs,
    makeSimpleTemplateFD,
    makeTemplateFD,
    copySrcTmplAsIs,
    srcDirInServerTemplatesDir,
    asTmplFile,
    asTmplSrcFile,
    asServerFile,
    asServerSrcFile,
    ServerRootDir,
    ServerSrcDir,
    ServerTemplatesDir,
    ServerTemplatesSrcDir,
  )
where

import qualified Data.Aeson as Aeson
import Wasp.Generator.Common (ProjectRootDir)
import Wasp.Generator.FileDraft (FileDraft, createTemplateFileDraft)
import Wasp.Generator.Templates (TemplatesDir)
import StrongPath (Dir, File', Path', Rel, reldir, (</>))
import qualified StrongPath as SP
import Wasp.Wasp (Wasp)

data ServerRootDir

data ServerSrcDir

data ServerTemplatesDir

data ServerTemplatesSrcDir

asTmplFile :: Path' (Rel d) File' -> Path' (Rel ServerTemplatesDir) File'
asTmplFile = SP.castRel

asTmplSrcFile :: Path' (Rel d) File' -> Path' (Rel ServerTemplatesSrcDir) File'
asTmplSrcFile = SP.castRel

asServerFile :: Path' (Rel d) File' -> Path' (Rel ServerRootDir) File'
asServerFile = SP.castRel

asServerSrcFile :: Path' (Rel d) File' -> Path' (Rel ServerSrcDir) File'
asServerSrcFile = SP.castRel

-- * Paths

-- | Path where server root dir is generated.
serverRootDirInProjectRootDir :: Path' (Rel ProjectRootDir) (Dir ServerRootDir)
serverRootDirInProjectRootDir = [reldir|server|]

-- | Path to generated server src/ directory.
serverSrcDirInServerRootDir :: Path' (Rel ServerRootDir) (Dir ServerSrcDir)
serverSrcDirInServerRootDir = [reldir|src|]

serverSrcDirInProjectRootDir :: Path' (Rel ProjectRootDir) (Dir ServerSrcDir)
serverSrcDirInProjectRootDir = serverRootDirInProjectRootDir </> serverSrcDirInServerRootDir

-- * Templates

copyTmplAsIs :: Path' (Rel ServerTemplatesDir) File' -> FileDraft
copyTmplAsIs srcPath = makeTemplateFD srcPath dstPath Nothing
  where
    dstPath = SP.castRel srcPath :: Path' (Rel ServerRootDir) File'

makeSimpleTemplateFD :: Path' (Rel ServerTemplatesDir) File' -> Wasp -> FileDraft
makeSimpleTemplateFD srcPath wasp = makeTemplateFD srcPath dstPath (Just $ Aeson.toJSON wasp)
  where
    dstPath = SP.castRel srcPath :: Path' (Rel ServerRootDir) File'

makeTemplateFD ::
  Path' (Rel ServerTemplatesDir) File' ->
  Path' (Rel ServerRootDir) File' ->
  Maybe Aeson.Value ->
  FileDraft
makeTemplateFD relSrcPath relDstPath tmplData =
  createTemplateFileDraft
    (serverRootDirInProjectRootDir </> relDstPath)
    (serverTemplatesDirInTemplatesDir </> relSrcPath)
    tmplData

copySrcTmplAsIs :: Path' (Rel ServerTemplatesSrcDir) File' -> FileDraft
copySrcTmplAsIs pathInTemplatesSrcDir = makeTemplateFD srcPath dstPath Nothing
  where
    srcPath = srcDirInServerTemplatesDir </> pathInTemplatesSrcDir
    dstPath =
      serverSrcDirInServerRootDir
        </> (SP.castRel pathInTemplatesSrcDir :: Path' (Rel ServerSrcDir) File')

-- | Path where server app templates reside.
serverTemplatesDirInTemplatesDir :: Path' (Rel TemplatesDir) (Dir ServerTemplatesDir)
serverTemplatesDirInTemplatesDir = [reldir|server|]

srcDirInServerTemplatesDir :: Path' (Rel ServerTemplatesDir) (Dir ServerTemplatesSrcDir)
srcDirInServerTemplatesDir = [reldir|src|]
