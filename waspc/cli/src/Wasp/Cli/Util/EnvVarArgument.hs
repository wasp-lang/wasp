module Wasp.Cli.Util.EnvVarArgument
  ( envVarReader,
    envVarFromString,
    EnvVarFileArgument,
    fromFilePath,
    WorkingDir,
    envVarFileReader,
    readEnvVarFile,
  )
where

import Options.Applicative (ReadM, eitherReader, str)
import StrongPath (Abs, File, Path')
import StrongPath.FilePath (parseAbsFile)
import System.Directory (makeAbsolute)
import Wasp.Cli.FileSystem (WorkingDir)
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
envVarFileReader = fromFilePath <$> str

fromFilePath :: FilePath -> EnvVarFileArgument
fromFilePath filePath =
  GetPathAction (makeAbsolute filePath >>= parseAbsFile)

-- Paths passed as arguments to a CLI are conventionally either absolute paths,
-- or paths relative to the current working directory. We need IO to resolve
-- which kind of path it is, but don't want any unsafe transformations in the
-- meantime; so we make this type opaque until we have access to the IO monad.
newtype EnvVarFileArgument = GetPathAction (IO (Path' Abs (File ())))

readEnvVarFile :: EnvVarFileArgument -> IO [EnvVar]
readEnvVarFile (GetPathAction getPath) = getPath >>= parseDotEnvFile
