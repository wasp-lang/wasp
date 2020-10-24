module StrongPath.Internal where

import           Control.Monad.Catch     (MonadThrow)
import qualified Path                    as P
import qualified Path.Posix              as PP
import qualified Path.Windows            as PW
import qualified System.FilePath.Posix   as FPP
import qualified System.FilePath.Windows as FPW


-- | s -> standard, b -> base, t -> type
data Path' s b t
    -- System
    = RelDir  (P.Path P.Rel P.Dir) RelPathPrefix
    | RelFile (P.Path P.Rel P.File) RelPathPrefix
    | AbsDir  (P.Path P.Abs P.Dir)
    | AbsFile (P.Path P.Abs P.File)
    -- Windows
    | RelDirW  (PW.Path PW.Rel PW.Dir) RelPathPrefix
    | RelFileW (PW.Path PW.Rel PW.File) RelPathPrefix
    | AbsDirW  (PW.Path PW.Abs PW.Dir)
    | AbsFileW (PW.Path PW.Abs PW.File)
    -- Posix
    | RelDirP  (PP.Path PP.Rel PP.Dir) RelPathPrefix
    | RelFileP (PP.Path PP.Rel PP.File) RelPathPrefix
    | AbsDirP  (PP.Path PP.Abs PP.Dir)
    | AbsFileP (PP.Path PP.Abs PP.File)
    deriving (Show, Eq)

data RelPathPrefix = ParentDir Int -- ^ ../, Int saying how many times it repeats.
                   | NoPrefix
    deriving (Show, Eq)

type Path = Path' System

-- | base
data Abs
data Rel dir

-- | type
data Dir dir
data File' file

type File = File' ()

-- | standard
data System -- Depends on the platform, it is either Posix or Windows.
data Windows
data Posix


parseRelFP :: MonadThrow m
    => (P.Path pb pt -> RelPathPrefix -> Path' s (Rel d) t)
    -> [Char]
    -> (FilePath -> m (P.Path pb pt))
    -> FilePath
    -> m (Path' s (Rel d) t)
parseRelFP constructor validSeparators pathParser fp =
    let (prefix, fp') = extractRelPathPrefix validSeparators fp
        fp'' = if fp' == "" then "." else fp' -- Because Path Rel parsers can't handle just "".
    in (\p -> constructor p prefix) <$> pathParser fp''

-- | Extracts a multiple "../" from start of the file path.
--   If path is completely ../../.., also handles the last one.
--   NOTE: We don't normalize path in any way.
extractRelPathPrefix :: [Char] -> FilePath -> (RelPathPrefix, FilePath)
extractRelPathPrefix validSeparators path =
    let (n, path') = dropParentDirs path
    in (if n == 0 then NoPrefix else ParentDir n, path')
  where
    parentDirStrings :: [String]
    parentDirStrings  = [['.', '.', s] | s <- validSeparators]

    pathStartsWithParentDir :: FilePath -> Bool
    pathStartsWithParentDir p  = take 3 p `elem` parentDirStrings

    dropParentDirs :: FilePath -> (Int, FilePath)
    dropParentDirs p
        | pathStartsWithParentDir p = let (n, p') = dropParentDirs (drop 3 p)
                                      in (1 + n, p')
        | p == ".."                 = (1, "")
        | otherwise                 = (0, p)

-- NOTE: These three funtions, pathWinCombine... exist only to fix
--   Path.Windows.</> behaviour regarding concatenating '.' rel dirs
--   with other paths. While for Path.System and Path.Posix this concatenation
--   behaves as expected on Linux, Path.Windows behaves differently!
--   In more details:
--   [P.reldir|.|] P.</> [P.reldir|a|] results in [P.reldir|a|]
--   however
--   [PW.reldir|.|] PW.</> [PW.reldir|a|] results in [PW.reldir|.\\a|]
--   To summarize it, for System/Posix, Path behaves as:
--   . </> a = a
--   . </> . = .
--   a </> a = a
--   While for Windows, Path behaves as:
--   . </> a = .\a
--   . </> . = .\.
--   a </> . = a\.
--   which we don't want, we want it to behave same as for System/Posix.
--   That is why we handle these cases as special cases and then we let the Path.Windows.</>
--   do the rest of the work.
pathWinCombineRelDirAndRelFile :: PW.Path PW.Rel PW.Dir -> PW.Path PW.Rel PW.File -> PW.Path PW.Rel PW.File
pathWinCombineRelDirAndRelFile lp rp
    | PW.toFilePath lp == ['.', FPW.pathSeparator] = rp
    | otherwise = lp PW.</> rp
pathWinCombineRelDirAndRelDir :: PW.Path PW.Rel PW.Dir -> PW.Path PW.Rel PW.Dir -> PW.Path PW.Rel PW.Dir
pathWinCombineRelDirAndRelDir lp rp
    | PW.toFilePath lp == ['.', FPW.pathSeparator] = rp
    | PW.toFilePath rp == ['.', FPW.pathSeparator] = lp
    | otherwise = lp PW.</> rp
pathWinCombineAbsDirAndRelDir :: PW.Path PW.Abs PW.Dir -> PW.Path PW.Rel PW.Dir -> PW.Path PW.Abs PW.Dir
pathWinCombineAbsDirAndRelDir lp rp
    | PW.toFilePath rp == ['.', FPW.pathSeparator] = lp
    | otherwise = lp PW.</> rp

-- NOTE: Same as pathWinCombineRelDirAndRelFile but for Posix (Path has the same problem).
pathPosixCombineRelDirAndRelFile :: PP.Path PP.Rel PP.Dir -> PP.Path PP.Rel PP.File -> PP.Path PP.Rel PP.File
pathPosixCombineRelDirAndRelFile lp rp
    | PP.toFilePath lp == ['.', FPP.pathSeparator] = rp
    | otherwise = lp PP.</> rp
pathPosixCombineRelDirAndRelDir :: PP.Path PP.Rel PP.Dir -> PP.Path PP.Rel PP.Dir -> PP.Path PP.Rel PP.Dir
pathPosixCombineRelDirAndRelDir lp rp
    | PP.toFilePath lp == ['.', FPP.pathSeparator] = rp
    | PP.toFilePath rp == ['.', FPP.pathSeparator] = lp
    | otherwise = lp PP.</> rp
pathPosixCombineAbsDirAndRelDir :: PP.Path PP.Abs PP.Dir -> PP.Path PP.Rel PP.Dir -> PP.Path PP.Abs PP.Dir
pathPosixCombineAbsDirAndRelDir lp rp
    | PP.toFilePath rp == ['.', FPP.pathSeparator] = lp
    | otherwise = lp PP.</> rp

prefixNumParentDirs :: RelPathPrefix -> Int
prefixNumParentDirs NoPrefix      = 0
prefixNumParentDirs (ParentDir n) = n

relPathNumParentDirs :: Path' s (Rel r) t -> Int
relPathNumParentDirs = prefixNumParentDirs . relPathPrefix

relPathPrefix :: Path' s (Rel r) t -> RelPathPrefix
relPathPrefix sp = case sp of
    RelDir _ pr   -> pr
    RelFile _ pr  -> pr
    RelDirW _ pr  -> pr
    RelFileW _ pr -> pr
    RelDirP _ pr  -> pr
    RelFileP _ pr -> pr
    _             -> impossible

impossible :: a
impossible = error "This should be impossible."
