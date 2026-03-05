{-# LANGUAGE QuasiQuotes #-}

module Wasp.Project.ExternalConfig.ViteConfig
  ( validateViteConfig,
  )
where

import Data.List (isInfixOf)
import qualified Data.Text as T
import NeatInterpolation (trimming)
import StrongPath (Abs, Dir, File, Path', relfile, toFilePath, (</>))
import System.Directory (doesFileExist)
import Wasp.Project.Common
  ( CompileError,
    WaspProjectDir,
  )
import qualified Wasp.Util.IO as IOUtil

validateViteConfig :: Path' Abs (Dir WaspProjectDir) -> IO (Either CompileError ())
validateViteConfig waspDir =
  findExistingFile viteConfigCandidates >>= \case
    Nothing -> return $ Left fileNotFoundMessage
    Just path -> validatePluginImport path
  where
    viteConfigCandidates :: [Path' Abs (File ())]
    viteConfigCandidates =
      [ waspDir </> [relfile|vite.config.ts|],
        waspDir </> [relfile|vite.config.js|]
      ]

    findExistingFile :: [Path' Abs (File ())] -> IO (Maybe (Path' Abs (File ())))
    findExistingFile [] = return Nothing
    findExistingFile (f : fs) = do
      exists <- doesFileExist $ toFilePath f
      if exists then return (Just f) else findExistingFile fs

    validatePluginImport :: Path' Abs (File ()) -> IO (Either CompileError ())
    validatePluginImport viteConfigFile = do
      contents <- IOUtil.readFile viteConfigFile
      return $
        if waspPluginImportModule `isInfixOf` contents
          then Right ()
          else Left missingPluginImportMessage

    waspPluginImportModule :: String
    waspPluginImportModule = "wasp/client/vite"

    fileNotFoundMessage :: CompileError
    fileNotFoundMessage =
      T.unpack
        [trimming|
          Couldn't find \`vite.config.ts\` (or \`vite.config.js\`) in the project directory.
          Wasp requires a Vite config file with the \`wasp\` plugin configured.
          Read more: ${viteConfigDocsUrl}
        |]

    missingPluginImportMessage :: CompileError
    missingPluginImportMessage =
      T.unpack
        [trimming|
          Your Vite config file doesn't seem to import the Wasp Vite plugin from "wasp/client/vite".
          The `wasp` plugin is required for Wasp to work correctly.
          Read more: ${viteConfigDocsUrl}
        |]

    viteConfigDocsUrl :: T.Text
    viteConfigDocsUrl = "https://wasp.sh/docs/project/custom-vite-config#required-configuration"
