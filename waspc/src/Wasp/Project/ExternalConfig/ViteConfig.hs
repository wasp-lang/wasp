{-# LANGUAGE QuasiQuotes #-}

module Wasp.Project.ExternalConfig.ViteConfig
  ( validateViteConfig,
  )
where

import Data.List (isInfixOf)
import qualified Data.Text as T
import NeatInterpolation (trimming)
import StrongPath (Abs, Dir, File, Path', Rel, relfile, toFilePath, (</>))
import System.Directory (doesFileExist)
import Wasp.Project.Common
  ( CompileError,
    WaspProjectDir,
  )
import qualified Wasp.Util.IO as IOUtil

validateViteConfig :: Path' Abs (Dir WaspProjectDir) -> IO (Either CompileError ())
validateViteConfig waspDir =
  findExistingFile candidates >>= \case
    Nothing -> return $ Left fileNotFoundMessage
    Just path -> validatePluginImport path
  where
    candidates =
      [ waspDir </> [relfile|vite.config.ts|],
        waspDir </> [relfile|vite.config.js|]
      ]

    findExistingFile [] = return Nothing
    findExistingFile (f : fs) = do
      exists <- doesFileExist $ toFilePath f
      if exists then return (Just f) else findExistingFile fs

    validatePluginImport viteConfigFile = do
      contents <- IOUtil.readFile viteConfigFile
      return $
        if contents `contains` "wasp/client/vite"
          then Right ()
          else Left missingPluginImportMessage

    contains = flip isInfixOf

    fileNotFoundMessage =
      T.unpack
        [trimming|
          Couldn't find vite.config.ts (or vite.config.js) in the project directory.
          Wasp requires a Vite config file that uses the `wasp` plugin.
          Read more: ${viteConfigDocsUrl}
        |]

    missingPluginImportMessage =
      T.unpack
        [trimming|
          Your Vite config file doesn't seem to import the Wasp Vite plugin from "wasp/client/vite".
          The `wasp` plugin is required for Wasp to work correctly.
          Read more: ${viteConfigDocsUrl}
        |]

    viteConfigDocsUrl :: T.Text
    viteConfigDocsUrl = "https://wasp.sh/docs/project/custom-vite-config#required-configuration"
