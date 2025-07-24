module Wasp.Cli.Util.EnvVarArgument
  ( envVarReader,
    envVarFromString,
    EnvVarFileArgument,
    getEnvVarFilePath,
    envVarFileReader,
    readEnvVarFile,
  )
where

import Control.Monad ((>=>))
import Options.Applicative (ReadM, eitherReader, str)
import StrongPath (Abs, File, Path')
import StrongPath.FilePath (parseAbsFile)
import System.Directory (makeAbsolute)
import Wasp.Env (EnvVar, parseDotEnvFile)

envVarReader :: ReadM EnvVar
envVarReader = eitherReader envVarFromString

-- | Converts a string to an EnvVar, throwing an error if the string is not in
-- the correct format. The input format is expected to be "NAME=VALUE".
envVarFromString :: String -> Either String EnvVar
envVarFromString var =
  case break (== '=') var of
    ([], _) -> failure
    (name, '=' : value) -> Right (name, value)
    _ -> failure
  where
    failure = Left $ "Environment variable must be in the format NAME=VALUE: " ++ var

envVarFileReader :: ReadM EnvVarFileArgument
envVarFileReader = EnvVarFileArgument <$> str

-- Paths passed as arguments to a CLI are conventionally either absolute paths,
-- or paths relative to the current working directory. We need IO to resolve
-- which kind of path it is, but we also don't want any unsafe transformations
-- in the meantime; so we make this type opaque until we have access to the IO
-- monad.
newtype EnvVarFileArgument = EnvVarFileArgument FilePath
  deriving (Show, Eq)

getEnvVarFilePath :: EnvVarFileArgument -> IO (Path' Abs (File ()))
getEnvVarFilePath (EnvVarFileArgument filePath) = makeAbsolute filePath >>= parseAbsFile

readEnvVarFile :: EnvVarFileArgument -> IO [EnvVar]
readEnvVarFile = getEnvVarFilePath >=> parseDotEnvFile
