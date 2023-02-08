{-# LANGUAGE DeriveGeneric #-}

module Wasp.Cli.Command.Telemetry.User
  ( UserSignature (..),
    obtainUserSignature,
  )
where

import qualified Data.UUID.V4 as UUID
import StrongPath (Abs, Dir, File', Path', relfile)
import qualified StrongPath as SP
import qualified System.Environment as ENV
import Wasp.Cli.Command.Telemetry.Common (TelemetryCacheDir)
import Wasp.Util (checksumFromString, hexToString, orIfNothingM)
import qualified Wasp.Util.IO as IOUtil

-- Random, non-identifyable UUID used to represent user in analytics.
newtype UserSignature = UserSignature {_userSignatureValue :: String} deriving (Show)

obtainUserSignature :: Path' Abs (Dir TelemetryCacheDir) -> IO UserSignature
obtainUserSignature telemetryCacheDirPath =
  getUserSignatureFromEnv `orIfNothingM` readOrCreateUserSignatureFile telemetryCacheDirPath

readOrCreateUserSignatureFile :: Path' Abs (Dir TelemetryCacheDir) -> IO UserSignature
readOrCreateUserSignatureFile telemetryCacheDirPath = do
  let userSignatureFile = getUserSignatureFilePath telemetryCacheDirPath
  fileExists <- IOUtil.doesFileExist userSignatureFile
  UserSignature
    <$> if fileExists
      then IOUtil.readFile userSignatureFile
      else do
        userSignature <- show <$> UUID.nextRandom
        IOUtil.writeFile userSignatureFile userSignature
        return userSignature

getUserSignatureFilePath :: Path' Abs (Dir TelemetryCacheDir) -> Path' Abs File'
getUserSignatureFilePath telemetryCacheDir = telemetryCacheDir SP.</> [relfile|signature|]

getUserSignatureFromEnv :: IO (Maybe UserSignature)
getUserSignatureFromEnv =
  (fmap . fmap) (UserSignature . hexToString . checksumFromString) (ENV.lookupEnv "WASP_TELEMETRY_USER_ID")
