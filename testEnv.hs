#!/usr/bin/env stack
{- stack
     exec ghci
     --package pretty-simple
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
import Fixtures
