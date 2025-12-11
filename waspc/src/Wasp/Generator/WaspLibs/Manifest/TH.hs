{-# LANGUAGE TemplateHaskell #-}

module Wasp.Generator.WaspLibs.Manifest.TH
  ( loadWaspLibsManifest,
  )
where

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Strict as M
import Language.Haskell.TH (Exp, Q, runIO)
import Language.Haskell.TH.Syntax (qAddDependentFile)
import StrongPath (fromAbsFile, (</>))
import Wasp.Generator.WaspLibs.Common (getAbsLibsSourceDirPath, libsManifestPathInDataDir)

loadWaspLibsManifest :: Q Exp
loadWaspLibsManifest = do
  libsSourceDirPath <- runIO getAbsLibsSourceDirPath
  let manifestAbsPath = libsSourceDirPath </> libsManifestPathInDataDir
      manifestPathStr = fromAbsFile manifestAbsPath

  -- Add the manifest file as a dependency so recompilation happens when it changes.
  qAddDependentFile manifestPathStr

  manifestContents <- runIO $ BL.readFile manifestPathStr
  case Aeson.decode manifestContents of
    Nothing -> fail $ "Failed to parse manifest.json at " ++ manifestPathStr
    Just (manifest :: M.Map String String) -> [|manifest|]
