module Wasp.Cli.Command.Start.ServerRuntimeInputChange
  ( classifyServerRuntimeInputChange,
  )
where

import StrongPath (Dir, File', Path', Rel, relfile, (</>))
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

classifyServerRuntimeInputChange :: WatchCompileResult -> ServerRuntimeInputChange
classifyServerRuntimeInputChange watchCompileResult
  | any projectFileChangeMightAffectServerRuntime (_watchProjectFileChanges watchCompileResult) = ServerRuntimeInputMightHaveChanged
  | any generatedAppPathChangeMightAffectServerRuntime (_compileGeneratedAppPathChanges $ _watchCompileResult watchCompileResult) = ServerRuntimeInputMightHaveChanged
  | otherwise = NoServerRuntimeInputChange

projectFileChangeMightAffectServerRuntime :: ProjectFileChange -> Bool
projectFileChangeMightAffectServerRuntime (ProjectFileChange pathInProject) =
  case FP.splitDirectories pathInProject of
    srcDirName : _ ->
      srcDirName == projectSrcDirName
        && FP.takeExtension pathInProject `elem` serverRuntimeInputExtensions
    _ -> False

generatedAppPathChangeMightAffectServerRuntime :: GeneratedAppPathChange -> Bool
generatedAppPathChangeMightAffectServerRuntime (GeneratedAppPathWritten path) =
  generatedAppPathMightAffectServerRuntime path
generatedAppPathChangeMightAffectServerRuntime (GeneratedAppPathDeleted path) =
  generatedAppPathMightAffectServerRuntime path

generatedAppPathMightAffectServerRuntime :: FileOrDirPathRelativeTo GeneratedAppDir -> Bool
generatedAppPathMightAffectServerRuntime (Left file) =
  file == generatedServerEnvFile
    || (fileIsUnderGeneratedServerSrc file && FP.takeExtension (SP.fromRelFile file) `elem` serverRuntimeInputExtensions)
generatedAppPathMightAffectServerRuntime (Right dir) =
  dirIsUnderGeneratedServerSrc dir

fileIsUnderGeneratedServerSrc :: Path' (Rel GeneratedAppDir) File' -> Bool
fileIsUnderGeneratedServerSrc = pathIsUnderGeneratedServerSrc . SP.fromRelFile

dirIsUnderGeneratedServerSrc :: Path' (Rel GeneratedAppDir) (Dir dir) -> Bool
dirIsUnderGeneratedServerSrc = pathIsUnderGeneratedServerSrc . SP.fromRelDir

pathIsUnderGeneratedServerSrc :: FilePath -> Bool
pathIsUnderGeneratedServerSrc path = generatedServerSrcDirParts `isPrefixOfPathParts` FP.splitDirectories path

isPrefixOfPathParts :: [FilePath] -> [FilePath] -> Bool
isPrefixOfPathParts [] _ = True
isPrefixOfPathParts _ [] = False
isPrefixOfPathParts (prefixPart : prefixRest) (pathPart : pathRest) =
  prefixPart == pathPart && isPrefixOfPathParts prefixRest pathRest

projectSrcDirName :: FilePath
projectSrcDirName = FP.dropTrailingPathSeparator $ SP.fromRelDir srcDirInWaspProjectDir

generatedServerSrcDirParts :: [FilePath]
generatedServerSrcDirParts = FP.splitDirectories $ SP.fromRelDir ServerGenerator.Common.serverSrcDirInGeneratedAppDir

generatedServerEnvFile :: Path' (Rel GeneratedAppDir) File'
generatedServerEnvFile = ServerGenerator.Common.serverRootDirInGeneratedAppDir </> [relfile|.env|]

serverRuntimeInputExtensions :: [String]
serverRuntimeInputExtensions = [".ts", ".mts", ".js", ".mjs", ".json"]
