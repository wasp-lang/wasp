module Wasp.Generator.ServerGenerator.Common
  ( serverRootDirInGeneratedAppDir,
    serverSrcDirInServerRootDir,
    serverSrcDirInGeneratedAppDir,
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
    defaultServerUrl,
    defaultServerPort,
    clientUrlEnvVarName,
    serverUrlEnvVarName,
    libsRootDirFromServerDir,
    operationsRouteInRootRouter,
    healthRoutePath,
    webSocketRoutePath,
    dotEnvInServerRootDir,
  )
where

import qualified Data.Aeson as Aeson
import StrongPath (Dir, File', Path', Rel, reldir, relfile, (</>))
import qualified StrongPath as SP
import System.FilePath (splitExtension)
import Wasp.Generator.Common
  ( GeneratedAppComponentSrcDir,
    GeneratedAppDir,
    ServerRootDir,
    UniversalTemplatesDir,
    universalTemplatesDirInTemplatesDir,
  )
import Wasp.Generator.FileDraft (FileDraft, createTemplateFileDraft)
import Wasp.Generator.Templates (TemplatesDir)
import qualified Wasp.Generator.WaspLibs.Common as WaspLibsC
import Wasp.Util.StrongPath (invertRelDir)

data ServerSrcDir

data ServerTemplatesDir

data ServerTemplatesSrcDir

instance GeneratedAppComponentSrcDir ServerSrcDir

asTmplFile :: Path' (Rel d) File' -> Path' (Rel ServerTemplatesDir) File'
asTmplFile = SP.castRel

asTmplSrcFile :: Path' (Rel d) File' -> Path' (Rel ServerTemplatesSrcDir) File'
asTmplSrcFile = SP.castRel

asServerFile :: Path' (Rel d) File' -> Path' (Rel ServerRootDir) File'
asServerFile = SP.castRel

asServerSrcFile :: Path' (Rel d) File' -> Path' (Rel ServerSrcDir) File'
asServerSrcFile = SP.castRel

-- | Path where server root dir is generated.
serverRootDirInGeneratedAppDir :: Path' (Rel GeneratedAppDir) (Dir ServerRootDir)
serverRootDirInGeneratedAppDir = [reldir|server|]

-- | The env file Wasp generates for the server in development. It holds the
-- user's `.env.server` variables, plus the ones Wasp manages itself (the URL of
-- the development database, for example).
dotEnvInServerRootDir :: Path' (Rel ServerRootDir) File'
dotEnvInServerRootDir = [relfile|.env|]

-- | Path to generated server src/ directory.
serverSrcDirInServerRootDir :: Path' (Rel ServerRootDir) (Dir ServerSrcDir)
serverSrcDirInServerRootDir = [reldir|src|]

serverSrcDirInGeneratedAppDir :: Path' (Rel GeneratedAppDir) (Dir ServerSrcDir)
serverSrcDirInGeneratedAppDir = serverRootDirInGeneratedAppDir </> serverSrcDirInServerRootDir

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
mkTmplFdWithData relSrcPath = mkTmplFdWithDstAndData relSrcPath dstPath
  where
    dstPath = SP.castRel relSrcPath :: Path' (Rel ServerRootDir) File'

mkTmplFdWithDstAndData ::
  Path' (Rel ServerTemplatesDir) File' ->
  Path' (Rel ServerRootDir) File' ->
  Maybe Aeson.Value ->
  FileDraft
mkTmplFdWithDstAndData relSrcPath relDstPath =
  createTemplateFileDraft
    (serverRootDirInGeneratedAppDir </> relDstPath)
    (serverTemplatesDirInTemplatesDir </> relSrcPath)

mkUniversalTmplFdWithDst ::
  Path' (Rel UniversalTemplatesDir) File' ->
  Path' (Rel ServerRootDir) File' ->
  FileDraft
mkUniversalTmplFdWithDst relSrcPath relDstPath =
  createTemplateFileDraft
    (serverRootDirInGeneratedAppDir </> relDstPath)
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

clientUrlEnvVarName :: String
clientUrlEnvVarName = "WASP_WEB_CLIENT_URL"

serverUrlEnvVarName :: String
serverUrlEnvVarName = "WASP_SERVER_URL"

-- | The port the built app listens on, unless its deployment says otherwise
-- (with @PORT@). In development, the app is served on the Vite server's port
-- instead (see @defaultClientPort@).
defaultServerPort :: Int
defaultServerPort = 3001

-- | Where the built app is served from, when it runs locally (@wasp build
-- start@) on its default port.
defaultServerUrl :: String
defaultServerUrl = "http://localhost:" ++ show defaultServerPort

operationsRouteInRootRouter :: String
operationsRouteInRootRouter = "operations"

-- | The route deployments can use to check that the server is up. It used to be
-- @\/@, which now belongs to the app's pages.
healthRoutePath :: String
healthRoutePath = "/_wasp/health"

-- | The route the app's websocket connections are upgraded on.
webSocketRoutePath :: String
webSocketRoutePath = "/_wasp/ws"

libsRootDirFromServerDir :: Path' (Rel ServerRootDir) (Dir WaspLibsC.LibsRootDir)
libsRootDirFromServerDir = invertRelDir serverRootDirInGeneratedAppDir </> WaspLibsC.libsRootDirInGeneratedAppDir
