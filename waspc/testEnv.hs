#!/usr/bin/env stack
{- stack
     exec ghci
     --package pretty-simple
     --package aeson-pretty
     --
     test/Fixtures.hs
-}

{-
 Stack script that provides a REPL testing environment for Wasp. Includes text fixtures
 and convenient packages out of the box. Use it when you want to interactively
 test a piece of your code.
 Run it either as an executable or with `stack SCRIPT_NAME`.
-}

import Text.Pretty.Simple (pPrint)
import Data.Aeson
import Data.Aeson.Encode.Pretty
import Data.ByteString.Lazy.Char8 as L

import Parser.Common (runWaspParser)
import Fixtures

-- | Prints any ToJSON instance, useful when testing parser.
printJSON :: ToJSON a => a -> IO ()
printJSON = L.putStrLn . encodePretty
