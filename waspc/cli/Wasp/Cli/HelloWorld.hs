module Wasp.Cli.HelloWorld (helloWorldTwice) where

import Wasp.Lib (hello)

helloWorldTwice :: String
helloWorldTwice = hello ++ " " ++ hello
