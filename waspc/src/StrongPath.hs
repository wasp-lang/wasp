module StrongPath
    ( Path
    , Abs, Rel, Dir, File
    , parseRelDir, parseRelFile, parseAbsDir, parseAbsFile
    , toFilePath
    , (</>)
    , castRel
    , parent
    , fromPathRelDir, fromPathRelFile, fromPathAbsDir, fromPathAbsFile
    , toPathRelDir, toPathRelFile, toPathAbsDir, toPathAbsFile
    ) where

import qualified Path as P
import Control.Monad.Catch (MonadThrow)


data Path b t = RelDir  (P.Path P.Rel P.Dir)
              | RelFile (P.Path P.Rel P.File)
              | AbsDir  (P.Path P.Abs P.Dir)
              | AbsFile (P.Path P.Abs P.File)
    deriving (Show, Eq)

data Abs
data Rel dir
data Dir dir
data File

-- TODO: We still depend on Path for creating hardcoded paths via generics. Any way to go around that?
--   Maybe implement our own mechanism for that, so that people don't have to know about / use Path?
--   This means we would implement our own [reldir|foobar|] stuff.

fromPathRelDir :: P.Path P.Rel P.Dir -> Path (Rel a) (Dir b)
fromPathRelDir p = RelDir p

fromPathRelFile :: P.Path P.Rel P.File -> Path (Rel a) File
fromPathRelFile p = RelFile p

fromPathAbsDir :: P.Path P.Abs P.Dir -> Path Abs (Dir a)
fromPathAbsDir p = AbsDir p

fromPathAbsFile :: P.Path P.Abs P.File -> Path Abs File
fromPathAbsFile p = AbsFile p


toPathRelDir :: Path (Rel a) (Dir b) -> P.Path P.Rel P.Dir
toPathRelDir (RelDir p) = p
toPathRelDir _ = impossible

toPathRelFile :: Path (Rel a) File -> P.Path P.Rel P.File
toPathRelFile (RelFile p) = p
toPathRelFile _ = impossible

toPathAbsDir :: Path Abs (Dir a) -> P.Path P.Abs P.Dir
toPathAbsDir (AbsDir p) = p
toPathAbsDir _ = impossible

toPathAbsFile :: Path Abs File -> P.Path P.Abs P.File
toPathAbsFile (AbsFile p) = p
toPathAbsFile _ = impossible


parseRelDir :: MonadThrow m => FilePath -> m (Path (Rel d1) (Dir d2))
parseRelDir fp = P.parseRelDir fp >>= return . fromPathRelDir

parseRelFile :: MonadThrow m => FilePath -> m (Path (Rel d) File)
parseRelFile fp = P.parseRelFile fp >>= return . fromPathRelFile

parseAbsDir :: MonadThrow m => FilePath -> m (Path Abs (Dir d))
parseAbsDir fp = P.parseAbsDir fp >>= return . fromPathAbsDir

parseAbsFile :: MonadThrow m => FilePath -> m (Path Abs File)
parseAbsFile fp = P.parseAbsFile fp >>= return . fromPathAbsFile


toFilePath :: Path b t -> FilePath
toFilePath (RelDir p) = P.toFilePath p
toFilePath (RelFile p) = P.toFilePath p
toFilePath (AbsDir p) = P.toFilePath p
toFilePath (AbsFile p) = P.toFilePath p


parent :: Path b t -> Path b (Dir d)
parent (RelDir p) = RelDir $ P.parent p
parent (RelFile p) = RelDir $ P.parent p
parent (AbsDir p) = AbsDir $ P.parent p
parent (AbsFile p) = AbsDir $ P.parent p


(</>) :: Path a (Dir d) -> Path (Rel d) c -> Path a c
(RelDir p1) </> (RelFile p2) = RelFile $ p1 P.</> p2
(RelDir p1) </> (RelDir p2) = RelDir $ p1 P.</> p2
(AbsDir p1) </> (RelFile p2) = AbsFile $ p1 P.</> p2
(AbsDir p1) </> (RelDir p2) = AbsDir $ p1 P.</> p2
_ </> _ = impossible


castRel :: Path (Rel d1) a -> Path (Rel d2) a
castRel (RelDir p) = RelDir p
castRel (RelFile p) = RelFile p
castRel _ = impossible


impossible :: a
impossible = error "This should be impossible."
