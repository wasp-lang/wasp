{-# LANGUAGE PartialTypeSignatures #-}
module StrongPath
    ( Path, Path'
    , Abs, Rel, Dir, File
    , System, Windows, Posix
    , parseRelDir, parseRelFile, parseAbsDir, parseAbsFile
    , parseRelDirW, parseRelFileW, parseAbsDirW, parseAbsFileW
    , parseRelDirP, parseRelFileP, parseAbsDirP, parseAbsFileP
    , fromPathRelDir, fromPathRelFile, fromPathAbsDir, fromPathAbsFile
    , fromPathRelDirW, fromPathRelFileW, fromPathAbsDirW, fromPathAbsFileW
    , fromPathRelDirP, fromPathRelFileP, fromPathAbsDirP, fromPathAbsFileP
    , toPathRelDir, toPathRelFile, toPathAbsDir, toPathAbsFile
    , toPathRelDirW, toPathRelFileW, toPathAbsDirW, toPathAbsFileW
    , toPathRelDirP, toPathRelFileP, toPathAbsDirP, toPathAbsFileP
    , toFilePath
    , (</>)
    , castRel
    , castDir
    , parent
    , relDirToPosix, relFileToPosix, relDirToPosix', relFileToPosix'
    -- For tests (TODO: Extract this stuff into Internal)
    , extractRelPathPrefix
    , RelPathPrefix (..)
    ) where

import           Control.Monad.Catch     (MonadThrow)
import           Data.List               (intercalate)
import           Data.Maybe              (fromJust)
import qualified Path                    as P
import qualified Path.Posix              as PP
import qualified Path.Windows            as PW
import qualified System.FilePath         as FP
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
data File

-- | standard
data System -- Depends on the platorm, it is either Posix or Windows.
data Windows
data Posix

-- TODO: We still depend on Path for creating hardcoded paths via generics. Any way to go around that?
--   Maybe implement our own mechanism for that, so that people don't have to know about / use Path?
--   This means we would implement our own [reldir|foobar|] stuff.

-- TODO: Can I use type classes and return type polymorhipsm to make all this shorter and reduce duplication?
-- class Path, and then I have PathWindows and PathPosix and PathSystem implement it, smth like that?
-- And then fromPathRelDir has polymorhic return type based on standard? I tried a little bit but it is complicated.

-- TODO: If there is no other solution to all this duplication, do some template haskell magic to simplify it.


-- Constructors
-- TODO: Although here I specify which exact type of Path (P.Path, PP.Path or PW.Path) is to be
--   given as first argument, I realized that if I do:
--     SP.fromPathRelDirW [P.reldir|test\file|]
--   compiler will not complain, although I put P instead of PW!
--   I am not sure why is this happening, we should figure it out.
--   This is not great because it means somebody can by accident construct
--   StrongPath that should be Windows but is really Posix.
--   Or can they? I am not sure if P.Path is just considered the same as PW.Path,
--   or P.relfile and PW.relfile and PP.relfile for some weird reason are polymorhic
--   in return type, or what is happening. I believe it is something close to the latter,
--   in which case it is less of a problem, but I am not sure.
--   Actually, it also does not complain if I do:
--     SP.fromPathRelFileP [P.reldir|test/file|]
--   so although I put reldir, and it should be relfile, it does not complain! How is that possible!?
--   If I put absdir, then it does complain, however not if I put reldir. Very weird.
--   NOTE: In Path, Path.Windows.Path and Path.Posix.Path are actually the same Path it seems
--     so compiler does not differentiate them (because they are all exporting the same module containing Path),
--     but Path.Windows.Rel and Path.Posix.Rel (and same for Abs/Dir/File) are not the same,
--     because they are done via Include mechanism.
fromPathRelDir   :: P.Path  P.Rel  P.Dir   -> Path' System  (Rel a) (Dir b)
fromPathRelFile  :: P.Path  P.Rel  P.File  -> Path' System  (Rel a) File
fromPathAbsDir   :: P.Path  P.Abs  P.Dir   -> Path' System  Abs     (Dir a)
fromPathAbsFile  :: P.Path  P.Abs  P.File  -> Path' System  Abs     File
fromPathRelDirW  :: PW.Path PW.Rel PW.Dir  -> Path' Windows (Rel a) (Dir b)
fromPathRelFileW :: PW.Path PW.Rel PW.File -> Path' Windows (Rel a) File
fromPathAbsDirW  :: PW.Path PW.Abs PW.Dir  -> Path' Windows Abs     (Dir a)
fromPathAbsFileW :: PW.Path PW.Abs PW.File -> Path' Windows Abs     File
fromPathRelDirP  :: PP.Path PP.Rel PP.Dir  -> Path' Posix   (Rel a) (Dir b)
fromPathRelFileP :: PP.Path PP.Rel PP.File -> Path' Posix   (Rel a) File
fromPathAbsDirP  :: PP.Path PP.Abs PP.Dir  -> Path' Posix   Abs     (Dir a)
fromPathAbsFileP :: PP.Path PP.Abs PP.File -> Path' Posix   Abs     File
---- System
fromPathRelDir p  = RelDir p NoPrefix
fromPathRelFile p  = RelFile p NoPrefix
fromPathAbsDir   = AbsDir
fromPathAbsFile  = AbsFile
---- Windows
fromPathRelDirW p = RelDirW p NoPrefix
fromPathRelFileW p = RelFileW p NoPrefix
fromPathAbsDirW  = AbsDirW
fromPathAbsFileW = AbsFileW
---- Posix
fromPathRelDirP p = RelDirP p NoPrefix
fromPathRelFileP p = RelFileP p NoPrefix
fromPathAbsDirP  = AbsDirP
fromPathAbsFileP = AbsFileP


-- TODO: Should I go with MonadThrow here instead of just throwing error? Probably!
--       I could, as error, return actual Path + info on how many ../ were there in StrongPath,
--       so user can recover from error and continue, if they wish.
-- Deconstructors
toPathRelDir   :: Path' System  (Rel a) (Dir b) -> P.Path  P.Rel  P.Dir
toPathRelFile  :: Path' System  (Rel a) File    -> P.Path  P.Rel  P.File
toPathAbsDir   :: Path' System  Abs     (Dir a) -> P.Path  P.Abs  P.Dir
toPathAbsFile  :: Path' System  Abs     File    -> P.Path  P.Abs  P.File
toPathRelDirW  :: Path' Windows (Rel a) (Dir b) -> PW.Path PW.Rel PW.Dir
toPathRelFileW :: Path' Windows (Rel a) File    -> PW.Path PW.Rel PW.File
toPathAbsDirW  :: Path' Windows Abs     (Dir a) -> PW.Path PW.Abs PW.Dir
toPathAbsFileW :: Path' Windows Abs     File    -> PW.Path PW.Abs PW.File
toPathRelDirP  :: Path' Posix   (Rel a) (Dir b) -> PP.Path PP.Rel PP.Dir
toPathRelFileP :: Path' Posix   (Rel a) File    -> PP.Path PP.Rel PP.File
toPathAbsDirP  :: Path' Posix   Abs     (Dir a) -> PP.Path PP.Abs PP.Dir
toPathAbsFileP :: Path' Posix   Abs     File    -> PP.Path PP.Abs PP.File
---- System
toPathRelDir (RelDir p NoPrefix) = p
toPathRelDir (RelDir _ _)        = relativeStrongPathWithPrefixToPathError
toPathRelDir _                   = impossible
toPathRelFile (RelFile p NoPrefix) = p
toPathRelFile (RelFile _ _)        = relativeStrongPathWithPrefixToPathError
toPathRelFile _                    = impossible
toPathAbsDir (AbsDir p) = p
toPathAbsDir _          = impossible
toPathAbsFile (AbsFile p) = p
toPathAbsFile _           = impossible
---- Windows
toPathRelDirW (RelDirW p NoPrefix) = p
toPathRelDirW (RelDirW _ _)        = relativeStrongPathWithPrefixToPathError
toPathRelDirW _                    = impossible
toPathRelFileW (RelFileW p NoPrefix) = p
toPathRelFileW (RelFileW _ _)        = relativeStrongPathWithPrefixToPathError
toPathRelFileW _                     = impossible
toPathAbsDirW (AbsDirW p) = p
toPathAbsDirW _           = impossible
toPathAbsFileW (AbsFileW p) = p
toPathAbsFileW _            = impossible
---- Posix
toPathRelDirP (RelDirP p NoPrefix) = p
toPathRelDirP (RelDirP _ _)        = relativeStrongPathWithPrefixToPathError
toPathRelDirP _                    = impossible
toPathRelFileP (RelFileP p NoPrefix) = p
toPathRelFileP (RelFileP _ _)        = relativeStrongPathWithPrefixToPathError
toPathRelFileP _                     = impossible
toPathAbsDirP (AbsDirP p) = p
toPathAbsDirP _           = impossible
toPathAbsFileP (AbsFileP p) = p
toPathAbsFileP _            = impossible

relativeStrongPathWithPrefixToPathError :: a
relativeStrongPathWithPrefixToPathError =
    error "Relative StrongPath.Path with prefix can't be converted into Path.Path."

-- Parsers
parseRelDir   :: MonadThrow m => FilePath -> m (Path' System  (Rel d1) (Dir d2))
parseRelFile  :: MonadThrow m => FilePath -> m (Path' System  (Rel d)  File)
parseAbsDir   :: MonadThrow m => FilePath -> m (Path' System  Abs      (Dir d))
parseAbsFile  :: MonadThrow m => FilePath -> m (Path' System  Abs      File)
parseRelDirW  :: MonadThrow m => FilePath -> m (Path' Windows (Rel d1) (Dir d2))
parseRelFileW :: MonadThrow m => FilePath -> m (Path' Windows (Rel d)  File)
parseAbsDirW  :: MonadThrow m => FilePath -> m (Path' Windows Abs      (Dir d))
parseAbsFileW :: MonadThrow m => FilePath -> m (Path' Windows Abs      File)
parseRelDirP  :: MonadThrow m => FilePath -> m (Path' Posix   (Rel d1) (Dir d2))
parseRelFileP :: MonadThrow m => FilePath -> m (Path' Posix   (Rel d)  File)
parseAbsDirP  :: MonadThrow m => FilePath -> m (Path' Posix   Abs      (Dir d))
parseAbsFileP :: MonadThrow m => FilePath -> m (Path' Posix   Abs      File)
---- System
parseRelDir  = parseRelFP RelDir P.parseRelDir
parseRelFile = parseRelFP RelFile P.parseRelFile
parseAbsDir fp = fromPathAbsDir <$> P.parseAbsDir fp
parseAbsFile fp = fromPathAbsFile <$> P.parseAbsFile fp
---- Windows
parseRelDirW = parseRelFP RelDirW PW.parseRelDir
parseRelFileW = parseRelFP RelFileW PW.parseRelFile -- TODO: This might not work, because parseRelFP will still not tolerate windows separator, even in this context. We should probably let parseRelFP know that it should use windows separator in such case. But at the end, will PW.parseRelFile behave as we expect? What do we even expect? Just to be clear, I am talking about case when we are on Posix system and we do parseRelFileW "..\\a\\b.txt" -> will this work as expected? What does even parseRelFileW mean -> parse from Posix/System into Windows? Because that is how it works right now. But shouldn't it mean -> parse from Windows (and maybe we allow also Posix) into Windows? Because that will not work on Posix, I think, as described above. But maybe PW.parseRelFile parses from Windows/Posix because it is coming from Path.Windows? I think we do want to be able to parse Windows with parseRelDirW.
-- Ok so reasoning: parseRelSmth should parse from System or Posix into System.
--                  parseRelSmthW should parse from Win or Posix into Windows.
--                  parseRelSmthP should parse from Posix into Posix.
--                  parseAbsSmth should parse from System into System.
--                  parseAbsSmthW should parse from Win into Windows.
--                  parseAbsSmthP should parse from Posix into Posix.
--  Yes I think this is how it should work!!! And it works like this in Path, I think? Or maybe not in some cases?
-- The only thing should be then: we should make extractRelPathPrefix, and therefore parseRelFP, take a list
-- of separators that they support.
parseAbsDirW fp = fromPathAbsDirW <$> PW.parseAbsDir fp
parseAbsFileW fp = fromPathAbsFileW <$> PW.parseAbsFile fp
---- Posix
parseRelDirP = parseRelFP RelDirP PP.parseRelDir
parseRelFileP = parseRelFP RelFileP PP.parseRelFile
parseAbsDirP fp = fromPathAbsDirP <$> PP.parseAbsDir fp
parseAbsFileP fp = fromPathAbsFileP <$> PP.parseAbsFile fp

parseRelFP :: MonadThrow m
    => (P.Path pb pt -> RelPathPrefix -> Path' s (Rel d) t)
    -> (FilePath -> m (P.Path pb pt))
    -> FilePath
    -> m (Path' s (Rel d) t)
parseRelFP constructor pathParser fp =
    let (prefix, fp') = extractRelPathPrefix fp
        fp'' = if fp' == "" then "." else fp' -- Because Path Rel parsers can't handle just "".
    in (\p -> constructor p prefix) <$> pathParser fp''

-- TODO: Write tests.
-- | Extracts a multiple "../" from start of the file path.
--   NOTE: We don't normalize path in any way.
extractRelPathPrefix :: FilePath -> (RelPathPrefix, FilePath)
extractRelPathPrefix path
    | pathStartsWithParentDir path  = let (n, p') = dropParentDirs path
                                      in (ParentDir n, p')
    | otherwise                     = (NoPrefix, path)
  where
    -- | NOTE: If I got it correct, this is how Path is doing it: they consider Posix
    --   separators to always be valid when parsing, independent of the system, and then
    --   also the separator of the system. So I am sticking to that here also.
    validSeparators :: [Char]
    validSeparators = [FP.pathSeparator, FPP.pathSeparator]

    parentDirStrings :: [String]
    parentDirStrings  = [['.', '.', s] | s <- validSeparators]

    pathStartsWithParentDir :: FilePath -> Bool
    pathStartsWithParentDir p  = take 3 p `elem` parentDirStrings

    dropParentDirs :: FilePath -> (Int, FilePath)
    dropParentDirs p
        | pathStartsWithParentDir p = let (n, p') = dropParentDirs (drop 3 p)
                                      in (1 + n, p')
        | otherwise                 = (0, p)


-- TODO: In Path, they have type constrained versions of toFilePath that work for
--   specific path type: fromAbsDir, fromRelDir, ... -> we could add smth similar.

toFilePath :: Path' s b t -> FilePath
toFilePath sp = case sp of
    ---- System
    RelDir p prefix  -> relPathToFilePath P.toFilePath FP.pathSeparator prefix p
    RelFile p prefix -> relPathToFilePath P.toFilePath FP.pathSeparator prefix p
    AbsDir p         -> P.toFilePath p
    AbsFile p        -> P.toFilePath p
    ---- Windows
    RelDirW p prefix ->  relPathToFilePath PW.toFilePath FPW.pathSeparator prefix p
    RelFileW p prefix ->  relPathToFilePath PW.toFilePath FPW.pathSeparator prefix p
    AbsDirW p ->  PW.toFilePath p
    AbsFileW p ->  PW.toFilePath p
    ---- Posix
    RelDirP p prefix ->  relPathToFilePath PP.toFilePath FPP.pathSeparator prefix p
    RelFileP p prefix ->  relPathToFilePath PP.toFilePath FPP.pathSeparator prefix p
    AbsDirP p ->  PP.toFilePath p
    AbsFileP p ->  PP.toFilePath p
  where
    relPathToFilePath pathToFilePath sep prefix path =
        combinePrefixWithPath (relPathPrefixToFilePath sep prefix) (pathToFilePath path)

    relPathPrefixToFilePath :: Char -> RelPathPrefix -> FilePath
    relPathPrefixToFilePath _ NoPrefix = ""
    relPathPrefixToFilePath sep (ParentDir n) =
        intercalate [sep] (replicate n "..") ++ [sep]

    -- Here we are assuming that prefix is either empty or ends with separator.
    combinePrefixWithPath :: String -> FilePath -> FilePath
    combinePrefixWithPath "" "."      = "." -- Path Rel can be "." so we have to handle that.
    combinePrefixWithPath prefix "."  = init prefix -- Drop last separator.
    combinePrefixWithPath "" "./"      = "./" -- Path Rel can be "./" so we have to handle that.
    combinePrefixWithPath prefix "./"  = prefix
    combinePrefixWithPath prefix path = prefix ++ path

-- TODO: Write tests:
--   any prefix, Path Rel File "file.txt" ~> any prefix, "."
--   NoPrefix, Path Rel Dir "." ~> "../", "."
--   any prefix, Path Rel Dir "a/b" ~> any prefix, "a"
--   and more...
-- | Either removes last entry or if there are no entries and just "../"s, adds one more "../".
--   If path is absolute root and it has no parent, it will return unchanged path, same like Path.
parent :: Path' s b t -> Path' s b (Dir d)
parent path = case path of
    ---- System
    RelDir p prefix   -> relDirPathParent RelDir P.parent p prefix
    RelFile p prefix  ->  RelDir (P.parent p) prefix
    AbsDir p          -> AbsDir $ P.parent p
    AbsFile p         -> AbsDir $ P.parent p
    ---- Windows
    RelDirW p prefix  -> relDirPathParent RelDirW PW.parent p prefix
    RelFileW p prefix -> RelDirW (PW.parent p) prefix
    AbsDirW p         -> AbsDirW $ PW.parent p
    AbsFileW p        -> AbsDirW $ PW.parent p
    ---- Posix
    RelDirP p prefix  -> relDirPathParent RelDirP PP.parent p prefix
    RelFileP p prefix ->  RelDirP (PP.parent p) prefix
    AbsDirP p         -> AbsDirP $ PP.parent p
    AbsFileP p        -> AbsDirP $ PP.parent p
  where
    -- NOTE: We need this special logic for RelDir, because if we have RelDir Path,
    --   it is possible that it is "." or smth like that and no parent can be obtained,
    --   in which case we want to add "../" to our prefix.
    --   For file though, we don't have that concern, because it will always be possible to
    --   get a parent, as per current Path implementation.
    relDirPathParent constructor pathParent p prefix =
        if pathParent p == p
        then let prefix' = case prefix of
                     ParentDir n -> ParentDir (n + 1)
                     NoPrefix    -> ParentDir 1
             in constructor p prefix'
        else let p' = pathParent p
             in constructor p' prefix


-- TODO: Tests.
-- | If right path has "../"s in prefix, they will be resolved. If left path is
--   absolute and right path has too many "../"s, they go "over" the root,
--   they will just be ignored, same like in Path.
(</>) :: Path' s a (Dir d) -> Path' s (Rel d) c -> Path' s a c
---- System
lsp@(RelDir _ _) </> (RelFile rp rprefix) =
    let (RelDir lp' lprefix') = iterate parent lsp !! numAncestors rprefix
    in RelFile (lp' P.</> rp) lprefix'
lsp@(RelDir _ _) </> (RelDir rp rprefix) =
    let (RelDir lp' lprefix') = iterate parent lsp !! numAncestors rprefix
    in RelDir (lp' P.</> rp) lprefix'
lsp@(AbsDir _) </> (RelFile rp rprefix) =
    let (AbsDir lp') = iterate parent lsp !! numAncestors rprefix
    in AbsFile (lp' P.</> rp)
lsp@(AbsDir _) </> (RelDir rp rprefix) =
    let (AbsDir lp') = iterate parent lsp !! numAncestors rprefix
    in AbsDir (lp' P.</> rp)
---- Windows
lsp@(RelDirW _ _) </> (RelFileW rp rprefix) =
    let (RelDirW lp' lprefix') = iterate parent lsp !! numAncestors rprefix
    in RelFileW (lp' PW.</> rp) lprefix'
lsp@(RelDirW _ _) </> (RelDirW rp rprefix) =
    let (RelDirW lp' lprefix') = iterate parent lsp !! numAncestors rprefix
    in RelDirW (lp' PW.</> rp) lprefix'
lsp@(AbsDirW _) </> (RelFileW rp rprefix) =
    let (AbsDirW lp') = iterate parent lsp !! numAncestors rprefix
    in AbsFileW (lp' PW.</> rp)
lsp@(AbsDirW _) </> (RelDirW rp rprefix) =
    let (AbsDirW lp') = iterate parent lsp !! numAncestors rprefix
    in AbsDirW (lp' PW.</> rp)
---- Posix
lsp@(RelDirP _ _) </> (RelFileP rp rprefix) =
    let (RelDirP lp' lprefix') = iterate parent lsp !! numAncestors rprefix
    in RelFileP (lp' PP.</> rp) lprefix'
lsp@(RelDirP _ _) </> (RelDirP rp rprefix) =
    let (RelDirP lp' lprefix') = iterate parent lsp !! numAncestors rprefix
    in RelDirP (lp' PP.</> rp) lprefix'
lsp@(AbsDirP _) </> (RelFileP rp rprefix) =
    let (AbsDirP lp') = iterate parent lsp !! numAncestors rprefix
    in AbsFileP (lp' PP.</> rp)
lsp@(AbsDirP _) </> (RelDirP rp rprefix) =
    let (AbsDirP lp') = iterate parent lsp !! numAncestors rprefix
    in AbsDirP (lp' PP.</> rp)
_ </> _ = impossible


numAncestors :: RelPathPrefix -> Int
numAncestors NoPrefix      = 0
numAncestors (ParentDir n) = n


castRel :: Path' s (Rel d1) a -> Path' s (Rel d2) a
---- System
castRel (RelDir p pr)   = RelDir p pr
castRel (RelFile p pr)  = RelFile p pr
---- Windows
castRel (RelDirW p pr)  = RelDirW p pr
castRel (RelFileW p pr) = RelFileW p pr
---- Posix
castRel (RelDirP p pr)  = RelDirP p pr
castRel (RelFileP p pr) = RelFileP p pr
castRel _               = impossible

castDir :: Path' s a (Dir d1) -> Path' s a (Dir d2)
---- System
castDir (AbsDir p)     = AbsDir p
castDir (RelDir p pr)  = RelDir p pr
---- Windows
castDir (AbsDirW p)    = AbsDirW p
castDir (RelDirW p pr) = RelDirW p pr
---- Posix
castDir (AbsDirP p)    = AbsDirP p
castDir (RelDirP p pr) = RelDirP p pr
castDir _              = impossible

-- TODO: I was not able to unite these two functions (`relDirToPosix` and `relFileToPosix`) into just `toPosix``
--   because Haskell did not believe me that I would be returning same "t" (Dir/File) in Path
--   as was in first argument. I wonder if there is easy way to go around that or if
--   we have to redo significant part of the StrongPath to be able to do smth like this.
-- | Converts relative path to posix by replacing current path separators with posix path separators.
--   Works well for "normal" relative paths like "a\b\c" (Win) or "a/b/c" (Posix).
--   If path is weird but still considered relative, like just "C:" on Win,
--   results can be unxpected, most likely resulting with error thrown.
--   If path is already Posix, it will not change.
relDirToPosix :: MonadThrow m => Path' s (Rel d1) (Dir d2) -> m (Path' Posix (Rel d1) (Dir d2))
relDirToPosix sp@(RelDir _ _)  = parseRelDirP $ FPP.joinPath $ FP.splitDirectories $ toFilePath sp
relDirToPosix sp@(RelDirW _ _) = parseRelDirP $ FPP.joinPath $ FPW.splitDirectories $ toFilePath sp
relDirToPosix (RelDirP p pr)   = return $ RelDirP p pr
relDirToPosix _                = impossible
relFileToPosix :: MonadThrow m => Path' s (Rel d1) File -> m (Path' Posix (Rel d1) File)
relFileToPosix sp@(RelFile _ _)  = parseRelFileP $ FPP.joinPath $ FP.splitDirectories $ toFilePath sp
relFileToPosix sp@(RelFileW _ _) = parseRelFileP $ FPP.joinPath $ FPW.splitDirectories $ toFilePath sp
relFileToPosix (RelFileP p pr)   = return $ RelFileP p pr
relFileToPosix _                 = impossible
-- TODO: Should I name these unsafe versions differently? Maybe relDirToPosixU?
-- Unsafe versions:
relDirToPosix' :: Path' s (Rel d1) (Dir d2) -> Path' Posix (Rel d1) (Dir d2)
relDirToPosix' = fromJust . relDirToPosix
relFileToPosix' :: Path' s (Rel d1) File -> Path' Posix (Rel d1) File
relFileToPosix' = fromJust . relFileToPosix


impossible :: a
impossible = error "This should be impossible."
