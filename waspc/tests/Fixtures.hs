module Fixtures where

import Data.Maybe (fromJust)
import qualified Path as P
import qualified StrongPath as SP
import qualified System.FilePath as FP

systemSPRoot :: SP.Path' SP.Abs (SP.Dir d)
systemSPRoot = fromJust $ SP.parseAbsDir systemFpRoot

systemPathRoot :: P.Path P.Abs P.Dir
systemPathRoot = fromJust $ P.parseAbsDir systemFpRoot

systemFpRoot :: FilePath
systemFpRoot = if FP.pathSeparator == '\\' then "C:\\" else "/"
