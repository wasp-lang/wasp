module Wasp.Util.ShellCommand where

quoteArg :: String -> String
quoteArg arg = "\"" ++ arg ++ "\""
