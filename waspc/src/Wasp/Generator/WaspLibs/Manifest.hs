{-# LANGUAGE TemplateHaskell #-}

module Wasp.Generator.WaspLibs.Manifest
  ( getTarballFilename,
  )
where

import qualified Data.Map.Strict as M
import qualified Wasp.Generator.WaspLibs.Manifest.TH as TH

type WaspLibPackageName = String

type WaspLibTarballFileName = String

libManifest :: M.Map WaspLibPackageName WaspLibTarballFileName
libManifest = $(TH.loadWaspLibsManifest)

getTarballFilename :: WaspLibPackageName -> Maybe WaspLibTarballFileName
getTarballFilename = flip M.lookup libManifest
