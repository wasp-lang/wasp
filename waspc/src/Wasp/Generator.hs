module Wasp.Generator
  ( writeWebAppCode,
    Wasp.Generator.Start.start,
  )
where

import Data.List.NonEmpty (toList)
import qualified Data.Text
import qualified Data.Text.IO
import Data.Time.Clock (getCurrentTime)
import qualified Data.Version
import qualified Paths_waspc
import StrongPath (Abs, Dir, Path', relfile, (</>))
import qualified StrongPath as SP
import Wasp.AppSpec (AppSpec)
import Wasp.Generator.Common (ProjectRootDir)
import Wasp.Generator.DbGenerator (genDb)
import Wasp.Generator.DockerGenerator (genDockerFiles)
import Wasp.Generator.FileDraft (FileDraft)
import Wasp.Generator.Monad (Generator, GeneratorError, GeneratorWarning, runGenerator)
import Wasp.Generator.ServerGenerator (genServer)
import Wasp.Generator.Setup (runSetup)
import qualified Wasp.Generator.Start
import Wasp.Generator.WebAppGenerator (generateWebApp)
import Wasp.Generator.WriteFileDrafts (writeFileDrafts)
import Wasp.Message (SendMessage)
import Wasp.Util ((<++>))

-- | Generates web app code from given Wasp and writes it to given destination directory.
--   If dstDir does not exist yet, it will be created.
--   If there are any errors returned, that means that generator failed but new code was possibly still written.
--   If no errors were returned, this means generator was successful and generated a new version of the project
--     (regardless of the warnings returned).
--   NOTE(martin): What if there is already smth in the dstDir? It is probably best
--     if we clean it up first? But we don't want this to end up with us deleting stuff
--     from user's machine. Maybe we just overwrite and we are good?
writeWebAppCode :: AppSpec -> Path' Abs (Dir ProjectRootDir) -> SendMessage -> IO ([GeneratorWarning], [GeneratorError])
writeWebAppCode spec dstDir sendMessage = do
  let (generatorWarnings, generatorResult) = runGenerator $ genApp spec

  case generatorResult of
    Left generatorErrors -> return (generatorWarnings, toList generatorErrors)
    Right fileDrafts -> do
      writeFileDrafts dstDir fileDrafts
      writeDotWaspInfo dstDir
      (setupGeneratorWarnings, setupGeneratorErrors) <- runSetup spec dstDir sendMessage
      return (generatorWarnings ++ setupGeneratorWarnings, setupGeneratorErrors)

genApp :: AppSpec -> Generator [FileDraft]
genApp spec =
  generateWebApp spec
    <++> genServer spec
    <++> genDb spec
    <++> genDockerFiles spec

-- | Writes .waspinfo, which contains some basic metadata about how/when wasp generated the code.
writeDotWaspInfo :: Path' Abs (Dir ProjectRootDir) -> IO ()
writeDotWaspInfo dstDir = do
  currentTime <- getCurrentTime
  let version = Data.Version.showVersion Paths_waspc.version
  let content = "Generated on " ++ show currentTime ++ " by waspc version " ++ show version ++ " ."
  let dstPath = dstDir </> [relfile|.waspinfo|]
  Data.Text.IO.writeFile (SP.toFilePath dstPath) (Data.Text.pack content)
