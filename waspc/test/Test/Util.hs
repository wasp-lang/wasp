module Test.Util
    ( posixToSystemFp
    ) where

import qualified Path as P
import qualified System.FilePath as FP

import Fixtures (fpRoot)

-- | Takes posix path and converts it into windows path if running on Windows or leaves as it is if on Unix.
posixToSystemFp :: FilePath -> FilePath
posixToSystemFp posixFp = maybeSystemRoot ++ systemFpRootless
    where
      maybeSystemRoot = if head posixFp == '/' then P.toFilePath fpRoot else ""
      posixFpRootless = if head posixFp == '/' then tail posixFp else posixFp
      systemFpRootless = map (\c -> if c == '/' then FP.pathSeparator else c) posixFpRootless
