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
    ) where

import           Control.Monad.Catch     (MonadThrow)
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
    = RelDir  (P.Path P.Rel P.Dir)
    | RelFile (P.Path P.Rel P.File)
    | AbsDir  (P.Path P.Abs P.Dir)
    | AbsFile (P.Path P.Abs P.File)
    -- Windows
    | RelDirW  (PW.Path PW.Rel PW.Dir)
    | RelFileW (PW.Path PW.Rel PW.File)
    | AbsDirW  (PW.Path PW.Abs PW.Dir)
    | AbsFileW (PW.Path PW.Abs PW.File)
    -- Posix
    | RelDirP  (PP.Path PP.Rel PP.Dir)
    | RelFileP (PP.Path PP.Rel PP.File)
    | AbsDirP  (PP.Path PP.Abs PP.Dir)
    | AbsFileP (PP.Path PP.Abs PP.File)
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
fromPathRelDir   = RelDir
fromPathRelFile  = RelFile
fromPathAbsDir   = AbsDir
fromPathAbsFile  = AbsFile
---- Windows
fromPathRelDirW  = RelDirW
fromPathRelFileW = RelFileW
fromPathAbsDirW  = AbsDirW
fromPathAbsFileW = AbsFileW
---- Posix
fromPathRelDirP  = RelDirP
fromPathRelFileP = RelFileP
fromPathAbsDirP  = AbsDirP
fromPathAbsFileP = AbsFileP


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
toPathRelDir (RelDir p) = p
toPathRelDir _          = impossible
toPathRelFile (RelFile p) = p
toPathRelFile _           = impossible
toPathAbsDir (AbsDir p) = p
toPathAbsDir _          = impossible
toPathAbsFile (AbsFile p) = p
toPathAbsFile _           = impossible
---- Windows
toPathRelDirW (RelDirW p) = p
toPathRelDirW _           = impossible
toPathRelFileW (RelFileW p) = p
toPathRelFileW _            = impossible
toPathAbsDirW (AbsDirW p) = p
toPathAbsDirW _           = impossible
toPathAbsFileW (AbsFileW p) = p
toPathAbsFileW _            = impossible
---- Posix
toPathRelDirP (RelDirP p) = p
toPathRelDirP _           = impossible
toPathRelFileP (RelFileP p) = p
toPathRelFileP _            = impossible
toPathAbsDirP (AbsDirP p) = p
toPathAbsDirP _           = impossible
toPathAbsFileP (AbsFileP p) = p
toPathAbsFileP _            = impossible


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
parseRelDir fp = fromPathRelDir <$> P.parseRelDir fp
parseRelFile fp = fromPathRelFile <$> P.parseRelFile fp
parseAbsDir fp = fromPathAbsDir <$> P.parseAbsDir fp
parseAbsFile fp = fromPathAbsFile <$> P.parseAbsFile fp
---- Windows
parseRelDirW fp = fromPathRelDirW <$> PW.parseRelDir fp
parseRelFileW fp = fromPathRelFileW <$> PW.parseRelFile fp
parseAbsDirW fp = fromPathAbsDirW <$> PW.parseAbsDir fp
parseAbsFileW fp = fromPathAbsFileW <$> PW.parseAbsFile fp
---- Posix
parseRelDirP fp = fromPathRelDirP <$> PP.parseRelDir fp
parseRelFileP fp = fromPathRelFileP <$> PP.parseRelFile fp
parseAbsDirP fp = fromPathAbsDirP <$> PP.parseAbsDir fp
parseAbsFileP fp = fromPathAbsFileP <$> PP.parseAbsFile fp


toFilePath :: Path' s b t -> FilePath
---- System
toFilePath (RelDir p)   = P.toFilePath p
toFilePath (RelFile p)  = P.toFilePath p
toFilePath (AbsDir p)   = P.toFilePath p
toFilePath (AbsFile p)  = P.toFilePath p
---- Windows
toFilePath (RelDirW p)  = PW.toFilePath p
toFilePath (RelFileW p) = PW.toFilePath p
toFilePath (AbsDirW p)  = PW.toFilePath p
toFilePath (AbsFileW p) = PW.toFilePath p
---- Posix
toFilePath (RelDirP p)  = PP.toFilePath p
toFilePath (RelFileP p) = PP.toFilePath p
toFilePath (AbsDirP p)  = PP.toFilePath p
toFilePath (AbsFileP p) = PP.toFilePath p


parent :: Path' s b t -> Path' s b (Dir d)
---- System
parent (RelDir p)   = RelDir $ P.parent p
parent (RelFile p)  = RelDir $ P.parent p
parent (AbsDir p)   = AbsDir $ P.parent p
parent (AbsFile p)  = AbsDir $ P.parent p
---- Windows
parent (RelDirW p)  = RelDirW $ PW.parent p
parent (RelFileW p) = RelDirW $ PW.parent p
parent (AbsDirW p)  = AbsDirW $ PW.parent p
parent (AbsFileW p) = AbsDirW $ PW.parent p
---- Posix
parent (RelDirP p)  = RelDirP $ PP.parent p
parent (RelFileP p) = RelDirP $ PP.parent p
parent (AbsDirP p)  = AbsDirP $ PP.parent p
parent (AbsFileP p) = AbsDirP $ PP.parent p


(</>) :: Path' s a (Dir d) -> Path' s (Rel d) c -> Path' s a c
---- System
(RelDir p1) </> (RelFile p2) = RelFile $ p1 P.</> p2
(RelDir p1) </> (RelDir p2) = RelDir $ p1 P.</> p2
(AbsDir p1) </> (RelFile p2) = AbsFile $ p1 P.</> p2
(AbsDir p1) </> (RelDir p2) = AbsDir $ p1 P.</> p2
---- Windows
(RelDirW p1) </> (RelFileW p2) = RelFileW $ p1 PW.</> p2
(RelDirW p1) </> (RelDirW p2) = RelDirW $ p1 PW.</> p2
(AbsDirW p1) </> (RelFileW p2) = AbsFileW $ p1 PW.</> p2
(AbsDirW p1) </> (RelDirW p2) = AbsDirW $ p1 PW.</> p2
---- Posix
(RelDirP p1) </> (RelFileP p2) = RelFileP $ p1 PP.</> p2
(RelDirP p1) </> (RelDirP p2) = RelDirP $ p1 PP.</> p2
(AbsDirP p1) </> (RelFileP p2) = AbsFileP $ p1 PP.</> p2
(AbsDirP p1) </> (RelDirP p2) = AbsDirP $ p1 PP.</> p2
_ </> _ = impossible


castRel :: Path' s (Rel d1) a -> Path' s (Rel d2) a
---- System
castRel (RelDir p)   = RelDir p
castRel (RelFile p)  = RelFile p
---- Windows
castRel (RelDirW p)  = RelDirW p
castRel (RelFileW p) = RelFileW p
---- Posix
castRel (RelDirP p)  = RelDirP p
castRel (RelFileP p) = RelFileP p
castRel _            = impossible

castDir :: Path' s a (Dir d1) -> Path' s a (Dir d2)
---- System
castDir (AbsDir p)  = AbsDir p
castDir (RelDir p)  = RelDir p
---- Windows
castDir (AbsDirW p) = AbsDirW p
castDir (RelDirW p) = RelDirW p
---- Posix
castDir (AbsDirP p) = AbsDirP p
castDir (RelDirP p) = RelDirP p
castDir _           = impossible

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
relDirToPosix sp@(RelDir _)  = parseRelDirP $ FPP.joinPath $ FP.splitDirectories $ toFilePath sp
relDirToPosix sp@(RelDirW _) = parseRelDirP $ FPP.joinPath $ FPW.splitDirectories $ toFilePath sp
relDirToPosix (RelDirP p)    = return $ RelDirP p
relDirToPosix _              = impossible
relFileToPosix :: MonadThrow m => Path' s (Rel d1) File -> m (Path' Posix (Rel d1) File)
relFileToPosix sp@(RelFile _)  = parseRelFileP $ FPP.joinPath $ FP.splitDirectories $ toFilePath sp
relFileToPosix sp@(RelFileW _) = parseRelFileP $ FPP.joinPath $ FPW.splitDirectories $ toFilePath sp
relFileToPosix (RelFileP p)    = return $ RelFileP p
relFileToPosix _              = impossible
-- TODO: Should I name these unsafe versions differently? Maybe relDirToPosixU?
-- Unsafe versions:
relDirToPosix' :: Path' s (Rel d1) (Dir d2) -> Path' Posix (Rel d1) (Dir d2)
relDirToPosix' = fromJust . relDirToPosix
relFileToPosix' :: Path' s (Rel d1) File -> Path' Posix (Rel d1) File
relFileToPosix' = fromJust . relFileToPosix


impossible :: a
impossible = error "This should be impossible."
