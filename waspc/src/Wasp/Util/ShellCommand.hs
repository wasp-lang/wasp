module Wasp.Util.ShellCommand where

data ShellCommandArg = Safe String | Unsafe String

instance Show ShellCommandArg where
  show (Safe arg) = arg
  show (Unsafe arg) = show arg

showShellArgs :: [ShellCommandArg] -> String
showShellArgs = unwords . map show

getShellArgValues :: [ShellCommandArg] -> [String]
getShellArgValues = map getShellArgValue
  where
    getShellArgValue (Safe arg) = arg
    getShellArgValue (Unsafe arg) = arg
