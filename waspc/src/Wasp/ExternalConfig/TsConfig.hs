{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Wasp.ExternalConfig.TsConfig
  ( TsConfig (..),
    analyzeTsConfigContent,
  )
where

import Control.Monad.Except
import Data.Aeson (FromJSON, parseJSON, withObject, (.:?))
import qualified Data.ByteString.Lazy.UTF8 as BS
import Data.Either.Extra (maybeToEither)
import qualified Data.Map as M
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
      -- We couldn't use the Generic deriving for this because of the _ prefix in the field name.
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
  lift $ print tsConfig
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
        [ validateTsConfigField "module" (_module compilerOptionsValues) "esnext",
          validateTsConfigField "target" (target compilerOptionsValues) "esnext",
          validateTsConfigField "moduleResolution" (moduleResolution compilerOptionsValues) "bundler",
          validateTsConfigField "jsx" (jsx compilerOptionsValues) "preserve",
          validateTsConfigField "strict" (strict compilerOptionsValues) True,
          validateTsConfigField "esModuleInterop" (esModuleInterop compilerOptionsValues) True,
          validateTsConfigField "lib" (lib compilerOptionsValues) ["dom", "dom.iterable", "esnext"],
          validateTsConfigField "allowJs" (allowJs compilerOptionsValues) True,
          validateTsConfigField "typeRoots" (typeRoots compilerOptionsValues) ["node_modules/@testing-library", "node_modules/@types"],
          validateTsConfigField "outDir" (outDir compilerOptionsValues) ".wasp/phantom"
        ]
    compilerOptionsValues = compilerOptions tsConfig

class ShowJs a where
  showJs :: a -> String

instance ShowJs String where
  showJs = show

instance ShowJs [String] where
  showJs = show

instance ShowJs Bool where
  showJs True = "true"
  showJs False = "false"

-- TODO: maybe use a more structured way of defining expected values, so that we can easily add new fields.
--     expectedValues :: M.Map FieldName TsConfigFieldType
--     expectedValues =
--       M.fromList
--         [ ("module", Str "esnext"),
--           ("target", Str "esnext"),
--           ("moduleResolution", Str "bundler"),
--           ("jsx", Str "preserve"),
--           ("strict", Bool True),
--           ("esModuleInterop", Bool True),
--           ("lib", StrList ["dom", "dom.iterable", "esnext"]),
--           ("allowJs", Bool True),
--           ("typeRoots", StrList ["node_modules/@testing-library", "node_modules/@types"]),
--           ("outDir", Str ".wasp/phantom")
--         ]

-- data TsConfigFieldType = Str String | Bool Bool | StrList [String]

-- instance Show TsConfigFieldType where
--   show :: TsConfigFieldType -> String
--   show (Str s) = s
--   show (Bool True) = show ("true" :: String)
--   show (Bool False) = show ("false" :: String)
--   show (StrList l) = show l

type FieldName = String

validateTsConfigField :: (Eq value, ShowJs value) => FieldName -> Maybe value -> value -> [CompileError]
validateTsConfigField fieldName Nothing expectedValue = [missingFieldErrorMessage]
  where
    missingFieldErrorMessage = unwords ["The", show fieldName, "field is missing in tsconfig.json. Expected value:", showJs expectedValue ++ "."]
validateTsConfigField fieldName (Just userProvidedValue) expectedValue =
  if userProvidedValue /= expectedValue
    then [invalidValueErrorMessage]
    else []
  where
    invalidValueErrorMessage = unwords ["Invalid value for the", show fieldName, "field in tsconfig.json file, expected:", showJs expectedValue ++ "."]
