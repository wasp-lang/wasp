module Wasp.Cli.Command.Start.ServerRuntimeInputChange
  ( classifyServerRuntimeInputChange,
  )
where

import Data.List (isPrefixOf)
import StrongPath (File', Path', Rel, relfile, (</>))
import qualified StrongPath as SP
import qualified System.FilePath as FP
import Wasp.Cli.Command.Compile (CompileResult (..))
import Wasp.Cli.Command.Watch (ProjectFileChange (..), WatchCompileResult (..))
import Wasp.Generator.Common (GeneratedAppDir)
import Wasp.Generator.FileDraft.Writeable (FileOrDirPathRelativeTo)
import qualified Wasp.Generator.ServerGenerator.Common as ServerGenerator.Common
import Wasp.Generator.ServerGenerator.ServerProcessSupervisor
  ( ServerRuntimeInputChange (..),
  )
import Wasp.Generator.WriteFileDrafts (GeneratedAppPathChange (..))
import Wasp.Project.Common (srcDirInWaspProjectDir)

-- Server runtime inputs are user `src` runtime files, generated `server/src`
-- runtime files, generated `server/src` directory deletes, and generated `server/.env`.
classifyServerRuntimeInputChange :: WatchCompileResult -> ServerRuntimeInputChange
classifyServerRuntimeInputChange watchCompileResult
  | any isServerRuntimeInputProjectFile (_watchProjectFileChanges watchCompileResult) = ServerRuntimeInputMightHaveChanged
  | any isServerRuntimeInputGeneratedAppChange (_compileGeneratedAppPathChanges $ _watchCompileResult watchCompileResult) = ServerRuntimeInputMightHaveChanged
  | otherwise = NoServerRuntimeInputChange

isServerRuntimeInputProjectFile :: ProjectFileChange -> Bool
isServerRuntimeInputProjectFile (ProjectFileChange pathInProject) =
  isFileInDirWithServerRuntimeInputExtension projectSrcDir pathInProject

isServerRuntimeInputGeneratedAppChange :: GeneratedAppPathChange -> Bool
isServerRuntimeInputGeneratedAppChange (GeneratedAppPathWritten path) =
  isServerRuntimeInputGeneratedAppPath path
isServerRuntimeInputGeneratedAppChange (GeneratedAppPathDeleted path) =
  isServerRuntimeInputGeneratedAppPath path

isServerRuntimeInputGeneratedAppPath :: FileOrDirPathRelativeTo GeneratedAppDir -> Bool
isServerRuntimeInputGeneratedAppPath (Left file) =
  file == generatedServerEnvFile
    || isGeneratedServerSrcRuntimeInputFile file
isServerRuntimeInputGeneratedAppPath (Right dir) =
  SP.fromRelDir dir `pathIsInDir` generatedServerSrcDir

isGeneratedServerSrcRuntimeInputFile :: Path' (Rel GeneratedAppDir) File' -> Bool
isGeneratedServerSrcRuntimeInputFile = isFileInDirWithServerRuntimeInputExtension generatedServerSrcDir . SP.fromRelFile

isFileInDirWithServerRuntimeInputExtension :: FilePath -> FilePath -> Bool
isFileInDirWithServerRuntimeInputExtension dir file =
  file `pathIsInDir` dir
    && hasServerRuntimeInputExtension file

hasServerRuntimeInputExtension :: FilePath -> Bool
hasServerRuntimeInputExtension path = FP.takeExtension path `elem` serverRuntimeInputExtensions

pathIsInDir :: FilePath -> FilePath -> Bool
pathIsInDir path dir = FP.splitDirectories dir `isPrefixOf` FP.splitDirectories path

projectSrcDir :: FilePath
projectSrcDir = FP.dropTrailingPathSeparator $ SP.fromRelDir srcDirInWaspProjectDir

generatedServerSrcDir :: FilePath
generatedServerSrcDir = FP.dropTrailingPathSeparator $ SP.fromRelDir ServerGenerator.Common.serverSrcDirInGeneratedAppDir

generatedServerEnvFile :: Path' (Rel GeneratedAppDir) File'
generatedServerEnvFile = ServerGenerator.Common.serverRootDirInGeneratedAppDir </> [relfile|.env|]

serverRuntimeInputExtensions :: [String]
serverRuntimeInputExtensions = [".ts", ".mts", ".js", ".mjs", ".json"]
