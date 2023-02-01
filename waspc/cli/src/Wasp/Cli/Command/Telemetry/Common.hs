module Wasp.Cli.Command.Telemetry.Common
  ( TelemetryCacheDir,
    ensureTelemetryCacheDirExists,
  )
where

import StrongPath (Abs, Dir, Path', reldir, (</>))
import qualified StrongPath as SP
import qualified System.Directory as SD
import Wasp.Cli.FileSystem
  ( UserCacheDir,
    getUserCacheDir,
    getWaspCacheDir,
  )

data TelemetryCacheDir

getTelemetryCacheDir :: Path' Abs (Dir UserCacheDir) -> Path' Abs (Dir TelemetryCacheDir)
getTelemetryCacheDir userCacheDirPath = getWaspCacheDir userCacheDirPath </> [reldir|telemetry|]

ensureTelemetryCacheDirExists :: IO (Path' Abs (Dir TelemetryCacheDir))
ensureTelemetryCacheDirExists = do
  userCacheDirPath <- getUserCacheDir
  SD.createDirectoryIfMissing False $ SP.fromAbsDir userCacheDirPath
  let telemetryCacheDir = getTelemetryCacheDir userCacheDirPath
  SD.createDirectoryIfMissing True $ SP.fromAbsDir telemetryCacheDir
  return telemetryCacheDir
