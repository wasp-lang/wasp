module Wasp.Generator
  ( writeWebAppCode,
    Wasp.Generator.Setup.setup,
    Wasp.Generator.Start.start,
  )
where

import Data.List.NonEmpty (toList)
import qualified Data.Text
import qualified Data.Text.IO
import Data.Time.Clock
import qualified Data.Version
import qualified Paths_waspc
import StrongPath (Abs, Dir, Path', relfile, (</>))
import qualified StrongPath as SP
import Wasp.AppSpec (AppSpec)
import Wasp.Generator.Common (ProjectRootDir)
import Wasp.Generator.DbGenerator (genDb)
import qualified Wasp.Generator.DbGenerator as DbGenerator
import Wasp.Generator.DockerGenerator (genDockerFiles)
import Wasp.Generator.FileDraft (FileDraft, write)
import Wasp.Generator.Monad (Generator, GeneratorError, GeneratorWarning, runGenerator)
import Wasp.Generator.ServerGenerator (genServer)
import qualified Wasp.Generator.ServerGenerator as ServerGenerator
import qualified Wasp.Generator.Setup
import qualified Wasp.Generator.Start
import Wasp.Generator.WebAppGenerator (generateWebApp)
import Wasp.Util ((<++>))

-- | Generates web app code from given Wasp and writes it to given destination directory.
--   If dstDir does not exist yet, it will be created.
--   If there are any errors returned, that means that generator failed and no new code was written.
--   If no errors were returned, this means generator was successful and generated a new version of the project
--     (regardless of the warnings returned).
--   NOTE(martin): What if there is already smth in the dstDir? It is probably best
--     if we clean it up first? But we don't want this to end up with us deleting stuff
--     from user's machine. Maybe we just overwrite and we are good?
writeWebAppCode :: AppSpec -> Path' Abs (Dir ProjectRootDir) -> IO ([GeneratorWarning], [GeneratorError])
writeWebAppCode spec dstDir = do
  let (generatorWarnings, generatorResult) = runGenerator $ genApp spec

  case generatorResult of
    Left generatorErrors -> return (generatorWarnings, toList generatorErrors)
    Right fileDrafts -> do
      preCleanup spec dstDir
      writeFileDrafts dstDir fileDrafts
      writeDotWaspInfo dstDir
      afterWriteWarnings <- afterWriteChecks spec dstDir
      return (generatorWarnings ++ afterWriteWarnings, [])

genApp :: AppSpec -> Generator [FileDraft]
genApp spec =
  generateWebApp spec
    <++> genServer spec
    <++> genDb spec
    <++> genDockerFiles spec

afterWriteChecks :: AppSpec -> Path' Abs (Dir ProjectRootDir) -> IO [GeneratorWarning]
afterWriteChecks spec dstDir = DbGenerator.afterWriteChecks spec dstDir

preCleanup :: AppSpec -> Path' Abs (Dir ProjectRootDir) -> IO ()
preCleanup spec dstDir = do
  ServerGenerator.preCleanup spec dstDir
  DbGenerator.preCleanup spec dstDir

-- | Writes file drafts while using given destination dir as root dir.
--   TODO(martin): We could/should parallelize this.
--     We could also skip writing files that are already on the disk with same checksum.
writeFileDrafts :: Path' Abs (Dir ProjectRootDir) -> [FileDraft] -> IO ()
writeFileDrafts dstDir = mapM_ (write dstDir)

-- | Writes .waspinfo, which contains some basic metadata about how/when wasp generated the code.
writeDotWaspInfo :: Path' Abs (Dir ProjectRootDir) -> IO ()
writeDotWaspInfo dstDir = do
  currentTime <- getCurrentTime
  let version = Data.Version.showVersion Paths_waspc.version
  let content = "Generated on " ++ show currentTime ++ " by waspc version " ++ show version ++ " ."
  let dstPath = dstDir </> [relfile|.waspinfo|]
  Data.Text.IO.writeFile (SP.toFilePath dstPath) (Data.Text.pack content)
