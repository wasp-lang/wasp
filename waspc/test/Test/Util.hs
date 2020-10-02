module Test.Util
    ( posixToSystemFp
    , posixToWindowsFp
    ) where

import qualified System.FilePath         as FP
import qualified System.FilePath.Windows as FPW

import           Fixtures                (systemFpRoot)


-- | Takes posix path and converts it into windows path if running on Windows or leaves as it is if on Unix.
posixToSystemFp :: FilePath -> FilePath
posixToSystemFp posixFp = maybeSystemRoot ++ systemFpRootless
    where
      maybeSystemRoot = if head posixFp == '/' then systemFpRoot else ""
      posixFpRootless = if head posixFp == '/' then tail posixFp else posixFp
      systemFpRootless = map (\c -> if c == '/' then FP.pathSeparator else c) posixFpRootless

-- | Takes posix path and converts it into windows path.
posixToWindowsFp :: FilePath -> FilePath
posixToWindowsFp posixFp = maybeWinRoot ++ winFpRootless
    where
      maybeWinRoot = if head posixFp == '/' then "C:\\" else ""
      posixFpRootless = if head posixFp == '/' then tail posixFp else posixFp
      winFpRootless = map (\c -> if c == '/' then FPW.pathSeparator else c) posixFpRootless
