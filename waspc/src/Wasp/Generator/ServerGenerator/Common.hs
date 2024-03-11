module Wasp.Generator.ServerGenerator.Common
  ( serverRootDirInProjectRootDir,
    serverSrcDirInServerRootDir,
    serverSrcDirInProjectRootDir,
    mkTmplFd,
    mkTmplFdWithDstAndData,
    mkSrcTmplFd,
    srcDirInServerTemplatesDir,
    asTmplFile,
    asTmplSrcFile,
    asServerFile,
    asServerSrcFile,
    toESModulesImportPath,
    mkUniversalTmplFdWithDst,
    mkTmplFdWithData,
    ServerRootDir,
    ServerSrcDir,
    ServerTemplatesDir,
    ServerTemplatesSrcDir,
    defaultDevServerUrl,
    defaultServerPort,
  )
where

import qualified Data.Aeson as Aeson
import StrongPath (Dir, File', Path', Rel, reldir, (</>))
import qualified StrongPath as SP
import System.FilePath (splitExtension)
import Wasp.Generator.Common
  ( GeneratedSrcDir,
    ProjectRootDir,
    ServerRootDir,
    UniversalTemplatesDir,
    universalTemplatesDirInTemplatesDir,
  )
import Wasp.Generator.FileDraft (FileDraft, createTemplateFileDraft)
import Wasp.Generator.Templates (TemplatesDir)

data ServerSrcDir

data ServerTemplatesDir

data ServerTemplatesSrcDir

instance GeneratedSrcDir ServerSrcDir

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

mkSrcTmplFd :: Path' (Rel ServerTemplatesSrcDir) File' -> FileDraft
mkSrcTmplFd pathInTemplatesSrcDir = mkTmplFdWithDstAndData srcPath dstPath Nothing
  where
    srcPath = srcDirInServerTemplatesDir </> pathInTemplatesSrcDir
    dstPath =
      serverSrcDirInServerRootDir
        </> (SP.castRel pathInTemplatesSrcDir :: Path' (Rel ServerSrcDir) File')

mkTmplFdWithData ::
  Path' (Rel ServerTemplatesDir) File' ->
  Maybe Aeson.Value ->
  FileDraft
mkTmplFdWithData relSrcPath tmplData = mkTmplFdWithDstAndData relSrcPath dstPath tmplData
  where
    dstPath = SP.castRel relSrcPath :: Path' (Rel ServerRootDir) File'

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

mkUniversalTmplFdWithDst ::
  Path' (Rel UniversalTemplatesDir) File' ->
  Path' (Rel ServerRootDir) File' ->
  FileDraft
mkUniversalTmplFdWithDst relSrcPath relDstPath =
  createTemplateFileDraft
    (serverRootDirInProjectRootDir </> relDstPath)
    (universalTemplatesDirInTemplatesDir </> relSrcPath)
    Nothing

-- | Path where server app templates reside.
serverTemplatesDirInTemplatesDir :: Path' (Rel TemplatesDir) (Dir ServerTemplatesDir)
serverTemplatesDirInTemplatesDir = [reldir|server|]

srcDirInServerTemplatesDir :: Path' (Rel ServerTemplatesDir) (Dir ServerTemplatesSrcDir)
srcDirInServerTemplatesDir = [reldir|src|]

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

defaultServerPort :: Int
defaultServerPort = 3001

defaultDevServerUrl :: String
defaultDevServerUrl = "http://localhost:" ++ show defaultServerPort
