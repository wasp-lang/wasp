module Lib
  ( compile,
    Generator.setup,
    Generator.start,
    ProjectRootDir,
    findWaspFile,
  )
where

import Common (WaspProjectDir)
import CompileOptions (CompileOptions)
import qualified CompileOptions
import Data.List (find, isSuffixOf)
import qualified ExternalCode
import qualified Generator
import Generator.Common (ProjectRootDir)
import qualified Parser
import StrongPath (Abs, Dir, File', Path', relfile)
import qualified StrongPath as SP
import qualified StrongPath.Path as SP.Path
import System.Directory (doesFileExist)
import qualified Util.IO
import Wasp (Wasp)
import qualified Wasp

type CompileError = String

compile ::
  Path' Abs (Dir WaspProjectDir) ->
  Path' Abs (Dir ProjectRootDir) ->
  CompileOptions ->
  IO (Either CompileError ())
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
    generateCode wasp = Generator.writeWebAppCode wasp outDir options >> return (Right ())

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
  files <- map SP.Path.fromPathRelFile . fst <$> Util.IO.listDirectory (SP.Path.toPathAbsDir waspDir)
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
