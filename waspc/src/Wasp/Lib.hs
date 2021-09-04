module Wasp.Lib
  ( compile,
    Generator.setup,
    Generator.start,
    ProjectRootDir,
    findWaspFile,
  )
where

import Data.List (find, isSuffixOf)
import StrongPath (Abs, Dir, File', Path', relfile)
import qualified StrongPath as SP
import System.Directory (doesFileExist)
import Wasp.Common (WaspProjectDir)
import Wasp.CompileOptions (CompileOptions)
import qualified Wasp.CompileOptions as CompileOptions
import qualified Wasp.ExternalCode as ExternalCode
import qualified Wasp.Generator as Generator
import Wasp.Generator.Common (ProjectRootDir)
import qualified Wasp.Parser as Parser
import qualified Wasp.Util.IO as Util.IO
import Wasp.Wasp (Wasp)
import qualified Wasp.Wasp as Wasp

type CompileError = String

compile ::
  Path' Abs (Dir WaspProjectDir) ->
  Path' Abs (Dir ProjectRootDir) ->
  CompileOptions ->
  IO (Either CompileError Wasp)
compile waspDir outDir options = do
  maybeWaspFile <- findWaspFile waspDir
  case maybeWaspFile of
    Nothing -> return $ Left "Couldn't find a single *.wasp file."
    Just waspFile -> do
      waspStr <- readFile (SP.toFilePath waspFile)

      case Parser.parseWasp waspStr of
        Left err -> return $ Left (show err)
        Right wasp -> do
          maybeDotEnvFile <- findDotEnvFile waspDir
          ( wasp
              `Wasp.setDotEnvFile` maybeDotEnvFile
              `enrichWaspASTBasedOnCompileOptions` options
            )
            >>= generateCode
  where
    generateCode wasp = Generator.writeWebAppCode wasp outDir options >> return (Right wasp)

enrichWaspASTBasedOnCompileOptions :: Wasp -> CompileOptions -> IO Wasp
enrichWaspASTBasedOnCompileOptions wasp options = do
  externalCodeFiles <- ExternalCode.readFiles (CompileOptions.externalCodeDirPath options)
  return
    ( wasp
        `Wasp.setExternalCodeFiles` externalCodeFiles
        `Wasp.setIsBuild` CompileOptions.isBuild options
    )

findWaspFile :: Path' Abs (Dir WaspProjectDir) -> IO (Maybe (Path' Abs File'))
findWaspFile waspDir = do
  files <- fst <$> Util.IO.listDirectory waspDir
  return $ (waspDir SP.</>) <$> find isWaspFile files
  where
    isWaspFile path =
      ".wasp" `isSuffixOf` SP.toFilePath path
        && (length (SP.toFilePath path) > length (".wasp" :: String))

findDotEnvFile :: Path' Abs (Dir WaspProjectDir) -> IO (Maybe (Path' Abs File'))
findDotEnvFile waspDir = do
  let dotEnvAbsPath = waspDir SP.</> [relfile|.env|]
  dotEnvExists <- doesFileExist (SP.toFilePath dotEnvAbsPath)
  return $ if dotEnvExists then Just dotEnvAbsPath else Nothing
