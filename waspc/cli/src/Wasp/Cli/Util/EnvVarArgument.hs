module Wasp.Cli.Util.EnvVarArgument
  ( EnvVarArgument (..),
    envVarFileArgumentParser,
    envVarLiteralArgumentParser,
    envVarFromString,
  )
where

import qualified Options.Applicative as Opt
import Wasp.Cli.Util.PathArgument (FilePathArgument, filePathReader)
import Wasp.Env (EnvVar)

data EnvVarArgument
  = EnvVarArgumentFile FilePathArgument
  | EnvVarArgumentLiteral EnvVar
  deriving (Show, Eq)

-- | Defines the parser for a flag that takes a file path argument and returns
-- an FilePathArgument that can be later parsed.
-- e.g. `--client-env-file path/to/file.env`.
envVarFileArgumentParser :: String -> String -> Opt.Parser EnvVarArgument
envVarFileArgumentParser fileOptionName helpText =
  EnvVarArgumentFile
    <$> Opt.option
      filePathReader
      ( Opt.long fileOptionName
          <> Opt.metavar "FILE_PATH"
          <> Opt.help helpText
          <> Opt.action "file"
      )

-- | Defines the parser for a flag that takes a "NAME=VALUE" argument and
-- returns an EnvVar.
-- e.g. `--client-env GOOGLE_KEY=1234`.
envVarLiteralArgumentParser :: Char -> String -> String -> Opt.Parser EnvVarArgument
envVarLiteralArgumentParser shortOptionName longOptionName helpText =
  EnvVarArgumentLiteral
    <$> Opt.option
      (Opt.eitherReader envVarFromString)
      ( Opt.long longOptionName
          <> Opt.short shortOptionName
          <> Opt.metavar "NAME=VALUE"
          <> Opt.help helpText
      )

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
