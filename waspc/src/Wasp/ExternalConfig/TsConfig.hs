{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}

module Wasp.ExternalConfig.TsConfig
  ( TsConfig (..),
    analyzeTsConfigContent,
  )
where

import Control.Monad.Except
import Data.Aeson (FromJSON, parseJSON, withObject, (.:?))
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

data CompilerOptions = CompilerOptions
  { _module :: !(Maybe String),
    target :: !(Maybe String),
    moduleResolution :: !(Maybe String),
    jsx :: !(Maybe String),
    strict :: !(Maybe Bool),
    esModuleInterop :: !(Maybe Bool),
    lib :: !(Maybe [String]),
    allowJs :: !(Maybe Bool),
    typeRoots :: !(Maybe [String]),
    outDir :: !(Maybe String)
  }
  deriving (Show)

instance FromJSON CompilerOptions where
  parseJSON = withObject "CompilerOptions" $ \v ->
    CompilerOptions
      -- We couldn't use the Generic deriving for this because of the _ prefix in the "module" field name.
      <$> v .:? "module"
      <*> v .:? "target"
      <*> v .:? "moduleResolution"
      <*> v .:? "jsx"
      <*> v .:? "strict"
      <*> v .:? "esModuleInterop"
      <*> v .:? "lib"
      <*> v .:? "allowJs"
      <*> v .:? "typeRoots"
      <*> v .:? "outDir"

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
    Left err -> return $ Left ["Failed to parse tsconfig.json file: " ++ err]

validateTsConfig :: TsConfig -> IO (Either [CompileError] TsConfig)
validateTsConfig tsConfig =
  return $
    if null tsConfigErrors
      then Right tsConfig
      else Left tsConfigErrors
  where
    tsConfigErrors =
      concat
        [ validateRequiredField "module" "esnext" (_module compilerOptionsValues),
          validateRequiredField "target" "esnext" (target compilerOptionsValues),
          validateRequiredField "moduleResolution" "bundler" (moduleResolution compilerOptionsValues),
          validateRequiredField "jsx" "preserve" (jsx compilerOptionsValues),
          validateRequiredField "strict" True (strict compilerOptionsValues),
          validateRequiredField "esModuleInterop" True (esModuleInterop compilerOptionsValues),
          validateRequiredField "lib" ["dom", "dom.iterable", "esnext"] (lib compilerOptionsValues),
          validateRequiredField "allowJs" True (allowJs compilerOptionsValues),
          validateRequiredField "typeRoots" ["node_modules/@testing-library", "node_modules/@types"] (typeRoots compilerOptionsValues),
          validateRequiredField "outDir" ".wasp/phantom" (outDir compilerOptionsValues)
        ]
    compilerOptionsValues = compilerOptions tsConfig

-- | Used to show expected values in error messages.
class ShowJs a where
  showJs :: a -> String

instance ShowJs String where
  showJs = show

instance ShowJs [String] where
  showJs = show

instance ShowJs Bool where
  showJs True = "true"
  showJs False = "false"

type FieldName = String

validateRequiredField :: (Eq value, ShowJs value) => FieldName -> value -> Maybe value -> [CompileError]
validateRequiredField fieldName expectedValue maybeUserProvidedValue = case maybeUserProvidedValue of
  Nothing -> [missingFieldErrorMessage]
  Just userProvidedValue ->
    if userProvidedValue /= expectedValue
      then [invalidValueErrorMessage]
      else []
  where
    invalidValueErrorMessage = unwords ["Invalid value for the", show fieldName, "field in tsconfig.json file, expected value:", showJs expectedValue ++ "."]
    missingFieldErrorMessage = unwords ["The", show fieldName, "field is missing in tsconfig.json. Expected value:", showJs expectedValue ++ "."]
