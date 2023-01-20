module Wasp.Cli.Command.Telemetry.Common
  ( TelemetryCacheDir,
    ensureTelemetryCacheDirExists,
    getTelemetryCacheDirPath,
    getWaspCacheDirPath,
  )
where

import StrongPath (Abs, Dir, Path', reldir)
import qualified StrongPath as SP
import qualified System.Directory as SD
import Wasp.Cli.Command.Utils.File (UserCacheDir, getUserCacheDirPath)

data TelemetryCacheDir

ensureTelemetryCacheDirExists :: IO (Path' Abs (Dir TelemetryCacheDir))
ensureTelemetryCacheDirExists = do
  userCacheDirPath <- getUserCacheDirPath
  SD.createDirectoryIfMissing False $ SP.fromAbsDir userCacheDirPath
  let telemetryCacheDirPath = getTelemetryCacheDirPath userCacheDirPath
  SD.createDirectoryIfMissing True $ SP.fromAbsDir telemetryCacheDirPath
  return telemetryCacheDirPath

getTelemetryCacheDirPath :: Path' Abs (Dir UserCacheDir) -> Path' Abs (Dir TelemetryCacheDir)
getTelemetryCacheDirPath userCacheDirPath = getWaspCacheDirPath userCacheDirPath SP.</> [reldir|telemetry|]

getWaspCacheDirPath :: Path' Abs (Dir UserCacheDir) -> Path' Abs (Dir TelemetryCacheDir)
getWaspCacheDirPath userCacheDirPath = userCacheDirPath SP.</> [reldir|wasp|]
