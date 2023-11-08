module Wasp.Util.ShellCommand where

import Data.String (IsString (fromString))

data ShellCommandArg = Raw String | Quoted String
  deriving (Eq)

instance Show ShellCommandArg where
  show (Raw arg) = arg
  show (Quoted arg) = "\"" ++ arg ++ "\""

instance IsString ShellCommandArg where
  fromString = Raw

showShellArgs :: [ShellCommandArg] -> String
showShellArgs = unwords . map show

getShellArgValues :: [ShellCommandArg] -> [String]
getShellArgValues = map getShellArgValue
  where
    getShellArgValue (Raw arg) = arg
    getShellArgValue (Quoted arg) = arg
