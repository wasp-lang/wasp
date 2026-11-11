{-# LANGUAGE QuasiQuotes #-}

module Wasp.Project.ExternalConfig.ViteConfig
  ( validateViteConfig,
  )
where

import qualified Data.Text as T
import NeatInterpolation (trimming)
import StrongPath (Abs, Dir, File', Path', relfile, toFilePath, (</>))
import System.Directory (doesFileExist)
import Validation (Validation (..))
import Wasp.Project.Common
  ( CompileError,
    WaspProjectDir,
  )
import qualified Wasp.Util.IO as IOUtil
import Wasp.Util.Terminal (styleLiteral)

validateViteConfig :: Path' Abs (Dir WaspProjectDir) -> IO (Validation [CompileError] ())
validateViteConfig waspDir =
  findExistingFile viteConfigCandidates >>= \case
    Nothing -> return $ Failure [fileNotFoundMessage]
    Just path -> validatePluginImport path
  where
    viteConfigCandidates :: [Path' Abs File']
    viteConfigCandidates =
      [ waspDir </> [relfile|vite.config.ts|],
        waspDir </> [relfile|vite.config.js|]
      ]

    findExistingFile :: [Path' Abs File'] -> IO (Maybe (Path' Abs File'))
    findExistingFile [] = return Nothing
    findExistingFile (f : fs) = do
      exists <- doesFileExist $ toFilePath f
      if exists then return (Just f) else findExistingFile fs

    validatePluginImport :: Path' Abs File' -> IO (Validation [CompileError] ())
    validatePluginImport viteConfigFile = do
      content <- IOUtil.readFileStrict viteConfigFile
      return $
        if waspPluginImportModule `T.isInfixOf` content
          then Success ()
          else Failure [missingPluginImportMessage]

    fileNotFoundMessage :: CompileError
    fileNotFoundMessage =
      T.unpack
        [trimming|
          Couldn't find ${viteConfigTsFile} (or ${viteConfigJsFile}) in the project directory.
          Wasp requires a Vite config file with the ${waspPluginName} plugin configured.
          Read more: ${viteConfigDocsUrl}
        |]

    missingPluginImportMessage :: CompileError
    missingPluginImportMessage =
      T.unpack
        [trimming|
          Your Vite config file doesn't seem to import the Wasp Vite plugin from ${styledWaspPluginImportModule}.
          The ${waspPluginName} plugin is required for Wasp to work correctly.
          Read more: ${viteConfigDocsUrl}
        |]

    waspPluginImportModule :: T.Text
    waspPluginImportModule = "wasp/client/vite"

    viteConfigTsFile, viteConfigJsFile, waspPluginName, styledWaspPluginImportModule, viteConfigDocsUrl :: T.Text
    viteConfigTsFile = T.pack $ styleLiteral "vite.config.ts"
    viteConfigJsFile = T.pack $ styleLiteral "vite.config.js"
    waspPluginName = T.pack $ styleLiteral "wasp"
    styledWaspPluginImportModule = T.pack $ styleLiteral $ T.unpack waspPluginImportModule
    viteConfigDocsUrl = T.pack $ styleLiteral "https://wasp.sh/docs/advanced/client-customization/custom-vite-config#required-configuration"
