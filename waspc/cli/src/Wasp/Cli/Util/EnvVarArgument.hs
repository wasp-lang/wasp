module Wasp.Cli.Util.EnvVarArgument
  ( envVarReader,
    envVarFromString,
    envVarInlineParser,
    envVarFileParser,
  )
where

import qualified Options.Applicative as Opt
import Wasp.Cli.Util.PathArgument (FilePathArgument, filePathReader)
import Wasp.Env (EnvVar)

-- | Defines the parser for a flag that takes a "NAME=VALUE" argument and
-- returns an EnvVar.
envVarInlineParser :: Char -> String -> String -> Opt.Parser EnvVar
envVarInlineParser shortOptionName longOptionName helpText =
  Opt.option envVarReader $
    Opt.long longOptionName
      <> Opt.short shortOptionName
      <> Opt.metavar "NAME=VALUE"
      <> Opt.help helpText

envVarReader :: Opt.ReadM EnvVar
envVarReader = Opt.eitherReader envVarFromString

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

-- | Defines the parser for a flag that takes a file path argument and returns
-- an FilePathArgument that can be later parsed.
envVarFileParser :: String -> String -> Opt.Parser FilePathArgument
envVarFileParser fileOptionName helpText =
  Opt.option filePathReader $
    Opt.long fileOptionName
      <> Opt.metavar "FILE_PATH"
      <> Opt.help helpText
      <> Opt.action "file"
