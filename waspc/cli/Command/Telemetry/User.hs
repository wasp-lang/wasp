{-# LANGUAGE DeriveGeneric #-}

module Command.Telemetry.User
    ( UserSignature(..)
    , readOrCreateUserSignatureFile
    ) where

import qualified Data.UUID.V4             as UUID
import           Path                     (relfile)
import qualified System.Directory         as SD

import           Command.Telemetry.Common (TelemetryCacheDir)
import           StrongPath               (Abs, Dir, File, Path)
import qualified StrongPath               as SP



-- Random, non-identifyable UUID used to represent user in analytics.
newtype UserSignature = UserSignature { _userSignatureValue :: String } deriving (Show)

readOrCreateUserSignatureFile :: Path Abs (Dir TelemetryCacheDir) -> IO UserSignature
readOrCreateUserSignatureFile telemetryCacheDirPath = do
    let filePath = getUserSignatureFilePath telemetryCacheDirPath
    let filePathFP = SP.toFilePath filePath
    fileExists <- SD.doesFileExist filePathFP
    UserSignature <$> if fileExists
        then readFile filePathFP
        else do userSignature <- show <$> UUID.nextRandom
                writeFile filePathFP userSignature
                return userSignature

getUserSignatureFilePath :: Path Abs (Dir TelemetryCacheDir) -> Path Abs File
getUserSignatureFilePath telemetryCacheDir = telemetryCacheDir SP.</> SP.fromPathRelFile [relfile|signature|]

