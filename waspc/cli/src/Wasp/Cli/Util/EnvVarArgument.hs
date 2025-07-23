module Wasp.Cli.Util.EnvVarArgument
  ( envVarReader,
    EnvVarFileArgument (..),
    fromFilePath,
    WorkingDir,
    envVarFileReader,
    readEnvVarFile,
  )
where

import Control.Arrow (ArrowChoice (left))
import Data.Function ((&))
import Data.Functor ((<&>))
import Options.Applicative (ReadM, eitherReader)
import qualified Path
import StrongPath (Abs, File, Path', Rel, (</>))
import qualified StrongPath.Path as SP.Path
import Wasp.Cli.FileSystem (WorkingDir, getWorkingDir)
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
envVarFileReader = eitherReader fromFilePath

fromFilePath :: FilePath -> Either String EnvVarFileArgument
fromFilePath str =
  Path.parseSomeFile str
    & left ((("Error while parsing path " <> str <> ": ") <>) . show)
    <&> \case
      Path.Abs absPath -> AbsoluteEnvVarFile $ SP.Path.fromPathAbsFile absPath
      Path.Rel relPath -> RelativeEnvVarFile $ SP.Path.fromPathRelFile relPath

-- Paths passed as arguments to a CLI are conventionally absolute paths, or
-- paths relative to the current working directory.
data EnvVarFileArgument
  = AbsoluteEnvVarFile (Path' Abs (File ()))
  | RelativeEnvVarFile (Path' (Rel WorkingDir) (File ()))

readEnvVarFile :: EnvVarFileArgument -> IO [EnvVar]
readEnvVarFile arg = toStrongPath arg >>= parseDotEnvFile
  where
    toStrongPath :: EnvVarFileArgument -> IO (Path' Abs (File ()))
    toStrongPath (AbsoluteEnvVarFile path) = return path
    toStrongPath (RelativeEnvVarFile path) = (</> path) <$> getWorkingDir
