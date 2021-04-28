module Lib
  ( compile,
    Generator.setup,
    Generator.start,
    ProjectRootDir,
  )
where

import Common (WaspProjectDir)
import CompileOptions (CompileOptions)
import qualified CompileOptions
import Control.Monad.IO.Class (liftIO)
import Data.List (find, isSuffixOf)
import qualified ExternalCode
import qualified Generator
import Generator.Common (ProjectRootDir)
import qualified Parser
import qualified Path as P
import StrongPath (Abs, Dir, Path)
import qualified StrongPath as SP
import System.Directory (doesFileExist)
import qualified Util.IO
import Wasp (Wasp)
import qualified Wasp

type CompileError = String

compile ::
  Path Abs (Dir WaspProjectDir) ->
  Path Abs (Dir ProjectRootDir) ->
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

findWaspFile :: Path Abs (Dir WaspProjectDir) -> IO (Maybe (Path Abs SP.File))
findWaspFile waspDir = do
  (files, _) <- liftIO $ Util.IO.listDirectory (SP.toPathAbsDir waspDir)
  return $ (waspDir SP.</>) . SP.fromPathRelFile <$> find isWaspFile files
  where
    isWaspFile :: P.Path P.Rel P.File -> Bool
    isWaspFile path =
      ".wasp" `isSuffixOf` P.toFilePath path
        && (length (P.toFilePath path) > length (".wasp" :: String))

findDotEnvFile :: Path Abs (Dir WaspProjectDir) -> IO (Maybe (Path Abs SP.File))
findDotEnvFile waspDir = do
  let dotEnvAbsPath = waspDir SP.</> SP.fromPathRelFile [P.relfile|.env|]
  dotEnvExists <- doesFileExist (SP.toFilePath dotEnvAbsPath)
  return $ if dotEnvExists then Just dotEnvAbsPath else Nothing
