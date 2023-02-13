module Wasp.Generator.ServerGenerator.Common
  ( serverRootDirInProjectRootDir,
    serverSrcDirInServerRootDir,
    serverSrcDirInProjectRootDir,
    mkTmplFd,
    mkTmplFdWithDstAndData,
    mkSrcTmplFd,
    dotEnvServer,
    srcDirInServerTemplatesDir,
    asTmplFile,
    asTmplSrcFile,
    asServerFile,
    asServerSrcFile,
    toESModulesImportPath,
    ServerSrcDir,
    ServerTemplatesDir,
    ServerTemplatesSrcDir,
  )
where

import qualified Data.Aeson as Aeson
import StrongPath (Dir, File', Path', Rel, reldir, relfile, (</>))
import qualified StrongPath as SP
import System.FilePath (splitExtension)
import Wasp.Common (WaspProjectDir)
import Wasp.Generator.Common (ProjectRootDir, ServerRootDir)
import Wasp.Generator.FileDraft (FileDraft, createTemplateFileDraft)
import Wasp.Generator.Templates (TemplatesDir)

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

-- | Path where server root dir is generated.
serverRootDirInProjectRootDir :: Path' (Rel ProjectRootDir) (Dir ServerRootDir)
serverRootDirInProjectRootDir = [reldir|server|]

-- | Path to generated server src/ directory.
serverSrcDirInServerRootDir :: Path' (Rel ServerRootDir) (Dir ServerSrcDir)
serverSrcDirInServerRootDir = [reldir|src|]

serverSrcDirInProjectRootDir :: Path' (Rel ProjectRootDir) (Dir ServerSrcDir)
serverSrcDirInProjectRootDir = serverRootDirInProjectRootDir </> serverSrcDirInServerRootDir

mkTmplFd :: Path' (Rel ServerTemplatesDir) File' -> FileDraft
mkTmplFd srcPath = mkTmplFdWithDstAndData srcPath dstPath Nothing
  where
    dstPath = SP.castRel srcPath :: Path' (Rel ServerRootDir) File'

mkTmplFdWithDstAndData ::
  Path' (Rel ServerTemplatesDir) File' ->
  Path' (Rel ServerRootDir) File' ->
  Maybe Aeson.Value ->
  FileDraft
mkTmplFdWithDstAndData relSrcPath relDstPath tmplData =
  createTemplateFileDraft
    (serverRootDirInProjectRootDir </> relDstPath)
    (serverTemplatesDirInTemplatesDir </> relSrcPath)
    tmplData

mkSrcTmplFd :: Path' (Rel ServerTemplatesSrcDir) File' -> FileDraft
mkSrcTmplFd pathInTemplatesSrcDir = mkTmplFdWithDstAndData srcPath dstPath Nothing
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

dotEnvServer :: Path' (SP.Rel WaspProjectDir) File'
dotEnvServer = [relfile|.env.server|]

-- Converts the real name of the source file (i.e., name on disk) into a name
-- that can be used in an ESNext import.
-- Specifically, when using the ESNext module system, all source files must be
-- imported with a '.js' extension (even if they are '.ts' files).
--
-- Details: https://github.com/wasp-lang/wasp/issues/812#issuecomment-1335579353
toESModulesImportPath :: FilePath -> FilePath
toESModulesImportPath = changeExtensionTo "js"
  where
    changeExtensionTo ext = (++ '.' : ext) . fst . splitExtension
