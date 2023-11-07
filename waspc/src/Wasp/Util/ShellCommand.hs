module Wasp.Util.ShellCommand where

data ShellCommandArg = Raw String | Quoted String

instance Show ShellCommandArg where
  show (Raw arg) = arg
  show (Quoted arg) = "\"" ++ arg ++ "\""

showShellArgs :: [ShellCommandArg] -> String
showShellArgs = unwords . map show

getShellArgValues :: [ShellCommandArg] -> [String]
getShellArgValues = map getShellArgValue
  where
    getShellArgValue (Raw arg) = arg
    getShellArgValue (Quoted arg) = arg
