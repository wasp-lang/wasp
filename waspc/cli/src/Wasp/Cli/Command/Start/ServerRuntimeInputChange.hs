module Wasp.Cli.Command.Start.ServerRuntimeInputChange
  ( classifyServerRuntimeInputChange,
  )
where

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
import Wasp.Util.Glob (GlobPatterns, compileGlobPatterns, dirAndDescendantsGlobs, matchesAnyGlob, recursiveFileGlobsWithExtensions)

classifyServerRuntimeInputChange :: WatchCompileResult -> ServerRuntimeInputChange
classifyServerRuntimeInputChange watchCompileResult
  | any changedProjectFileIsServerRuntimeInput changedProjectFiles = ServerRuntimeInputMightHaveChanged
  | any changedGeneratedPathIsServerRuntimeInput changedGeneratedPaths = ServerRuntimeInputMightHaveChanged
  | otherwise = NoServerRuntimeInputChange
  where
    changedProjectFiles = _watchProjectFileChanges watchCompileResult
    changedGeneratedPaths = _compileGeneratedAppPathChanges $ _watchCompileResult watchCompileResult

changedProjectFileIsServerRuntimeInput :: ProjectFileChange -> Bool
changedProjectFileIsServerRuntimeInput (ProjectFileChange pathInProject) =
  projectServerRuntimeInputFileGlobs `matchesAnyGlob` pathInProject

changedGeneratedPathIsServerRuntimeInput :: GeneratedAppPathChange -> Bool
changedGeneratedPathIsServerRuntimeInput (GeneratedAppPathWritten path) =
  generatedPathIsServerRuntimeInput path
changedGeneratedPathIsServerRuntimeInput (GeneratedAppPathDeleted path) =
  generatedPathIsServerRuntimeInput path

generatedPathIsServerRuntimeInput :: FileOrDirPathRelativeTo GeneratedAppDir -> Bool
generatedPathIsServerRuntimeInput (Left file) =
  generatedServerRuntimeInputFileGlobs `matchesAnyGlob` SP.fromRelFile file
generatedPathIsServerRuntimeInput (Right dir) =
  generatedServerRuntimeInputDirGlobs `matchesAnyGlob` FP.dropTrailingPathSeparator (SP.fromRelDir dir)

projectServerRuntimeInputFilePatterns :: [String]
projectServerRuntimeInputFilePatterns =
  recursiveFileGlobsWithExtensions projectSrcDir serverRuntimeInputFileExtensions

generatedServerRuntimeInputFilePatterns :: [String]
generatedServerRuntimeInputFilePatterns =
  generatedServerEnvFile
    : recursiveFileGlobsWithExtensions generatedServerSrcDir serverRuntimeInputFileExtensions

generatedServerRuntimeInputDirPatterns :: [String]
generatedServerRuntimeInputDirPatterns = dirAndDescendantsGlobs generatedServerSrcDir

projectServerRuntimeInputFileGlobs :: GlobPatterns
projectServerRuntimeInputFileGlobs = compileGlobPatterns projectServerRuntimeInputFilePatterns

generatedServerRuntimeInputFileGlobs :: GlobPatterns
generatedServerRuntimeInputFileGlobs = compileGlobPatterns generatedServerRuntimeInputFilePatterns

generatedServerRuntimeInputDirGlobs :: GlobPatterns
generatedServerRuntimeInputDirGlobs = compileGlobPatterns generatedServerRuntimeInputDirPatterns

projectSrcDir :: FilePath
projectSrcDir = FP.dropTrailingPathSeparator $ SP.fromRelDir srcDirInWaspProjectDir

generatedServerSrcDir :: FilePath
generatedServerSrcDir = FP.dropTrailingPathSeparator $ SP.fromRelDir ServerGenerator.Common.serverSrcDirInGeneratedAppDir

generatedServerEnvFile :: FilePath
generatedServerEnvFile = SP.fromRelDir ServerGenerator.Common.serverRootDirInGeneratedAppDir FP.</> ".env"

serverRuntimeInputFileExtensions :: [String]
serverRuntimeInputFileExtensions = [".ts", ".mts", ".js", ".mjs", ".json"]
