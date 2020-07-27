module StrongPath
    ( Path
    , Abs, File, Dir
    , (</>)
    , fromRelDir, fromRelFile, fromAbsDir, fromAbsFile
    , toRelDir, toRelFile, toAbsDir, toAbsFile
    ) where

import qualified Path as P


data Path from to = RelDir  (P.Path P.Rel P.Dir)
                  | RelFile (P.Path P.Rel P.File)
                  | AbsDir  (P.Path P.Abs P.Dir)
                  | AbsFile (P.Path P.Abs P.File)

data Abs
data Dir name
data File


fromRelDir :: P.Path P.Rel P.Dir -> Path (Dir a) (Dir b)
fromRelDir p = RelDir p

fromRelFile :: P.Path P.Rel P.File -> Path (Dir a) File
fromRelFile p = RelFile p

fromAbsDir :: P.Path P.Abs P.Dir -> Path Abs (Dir a)
fromAbsDir p = AbsDir p

fromAbsFile :: P.Path P.Abs P.File -> Path Abs File
fromAbsFile p = AbsFile p


toRelDir :: Path (Dir a) (Dir b) -> P.Path P.Rel P.Dir
toRelDir (RelDir p) = p
toRelDir _ = impossible

toRelFile :: Path (Dir a) File -> P.Path P.Rel P.File
toRelFile (RelFile p) = p
toRelFile _ = impossible

toAbsDir :: Path Abs (Dir a) -> P.Path P.Abs P.Dir
toAbsDir (AbsDir p) = p
toAbsDir _ = impossible

toAbsFile :: Path Abs File -> P.Path P.Abs P.File
toAbsFile (AbsFile p) = p
toAbsFile _ = impossible


(</>) :: Path a b -> Path b c -> Path a c
(RelDir p1) </> (RelFile p2) = RelFile $ p1 P.</> p2
(RelDir p1) </> (RelDir p2) = RelDir $ p1 P.</> p2
(AbsDir p1) </> (RelFile p2) = AbsFile $ p1 P.</> p2
(AbsDir p1) </> (RelDir p2) = AbsDir $ p1 P.</> p2
_ </> _ = impossible

impossible :: a
impossible = error "This should be impossible."
