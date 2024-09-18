{-# LANGUAGE DeriveGeneric #-}

module Wasp.ExternalConfig.TsConfig
  ( TsConfig (..),
    analyzeTsConfigContent,
  )
where

import Control.Monad.Except
import Data.Aeson (FromJSON)
import qualified Data.ByteString.Lazy.UTF8 as BS
import Data.Either.Extra (maybeToEither)
import GHC.Generics (Generic)
import StrongPath (Abs, Dir, File', Path', toFilePath)
import Wasp.Project.Common
  ( CompileError,
    WaspProjectDir,
    findFileInWaspProjectDir,
    tsConfigInWaspProjectDir,
  )
import qualified Wasp.Util.IO as IOUtil
import Wasp.Util.Json (parseJsonWithComments)

data TsConfig = TsConfig
  { compilerOptions :: !CompilerOptions
  }
  deriving (Show, Generic)

instance FromJSON TsConfig

-- TODO: define fields we want to validate
data CompilerOptions = CompilerOptions
  { target :: !String
  }
  deriving (Show, Generic)

instance FromJSON CompilerOptions

analyzeTsConfigContent :: Path' Abs (Dir WaspProjectDir) -> IO (Either [CompileError] TsConfig)
analyzeTsConfigContent waspDir = runExceptT $ do
  tsConfigFile <- ExceptT findTsConfigOrError
  tsConfig <- ExceptT $ readTsConfigFile tsConfigFile
  ExceptT $ validateTsConfig tsConfig
  where
    findTsConfigOrError = maybeToEither [fileNotFoundMessage] <$> findTsConfigFile waspDir
    fileNotFoundMessage = "Couldn't find the tsconfig.json file in the " ++ toFilePath waspDir ++ " directory"

findTsConfigFile :: Path' Abs (Dir WaspProjectDir) -> IO (Maybe (Path' Abs File'))
findTsConfigFile waspProjectDir = findFileInWaspProjectDir waspProjectDir tsConfigInWaspProjectDir

readTsConfigFile :: Path' Abs File' -> IO (Either [CompileError] TsConfig)
readTsConfigFile tsConfigFile = do
  tsConfigContent <- IOUtil.readFileBytes tsConfigFile

  parseResult <- parseJsonWithComments . BS.toString $ tsConfigContent

  case parseResult of
    Right tsConfig -> return $ Right tsConfig
    Left err -> return $ Left [err]

-- TODO: validate config fields
validateTsConfig :: TsConfig -> IO (Either [CompileError] TsConfig)
validateTsConfig tsConfig = return $ Right tsConfig
