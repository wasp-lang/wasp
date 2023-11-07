module Wasp.Util.ShellCommand where

data ShellCommandArg = RawArgument String | QuotedArgument String

instance Show ShellCommandArg where
  show (RawArgument arg) = arg
  show (QuotedArgument arg) = show arg

showShellArgs :: [ShellCommandArg] -> String
showShellArgs = unwords . map show

getShellArgValues :: [ShellCommandArg] -> [String]
getShellArgValues = map getShellArgValue
  where
    getShellArgValue (RawArgument arg) = arg
    getShellArgValue (QuotedArgument arg) = arg
