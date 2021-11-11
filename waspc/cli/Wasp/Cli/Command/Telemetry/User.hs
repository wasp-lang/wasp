{-# LANGUAGE DeriveGeneric #-}

module Wasp.Cli.Command.Telemetry.User
  ( UserSignature (..),
    readOrCreateUserSignatureFile,
  )
where

import Wasp.Cli.Command.Telemetry.Common (TelemetryCacheDir)
import qualified Data.UUID.V4 as UUID
import StrongPath (Abs, Dir, File', Path', relfile)
import qualified StrongPath as SP
import qualified System.Directory as SD

-- Random, non-identifyable UUID used to represent user in analytics.
newtype UserSignature = UserSignature {_userSignatureValue :: String} deriving (Show)

readOrCreateUserSignatureFile :: Path' Abs (Dir TelemetryCacheDir) -> IO UserSignature
readOrCreateUserSignatureFile telemetryCacheDirPath = do
  let filePath = getUserSignatureFilePath telemetryCacheDirPath
  let filePathFP = SP.fromAbsFile filePath
  fileExists <- SD.doesFileExist filePathFP
  UserSignature
    <$> if fileExists
      then readFile filePathFP
      else do
        userSignature <- show <$> UUID.nextRandom
        writeFile filePathFP userSignature
        return userSignature

getUserSignatureFilePath :: Path' Abs (Dir TelemetryCacheDir) -> Path' Abs File'
getUserSignatureFilePath telemetryCacheDir = telemetryCacheDir SP.</> [relfile|signature|]
