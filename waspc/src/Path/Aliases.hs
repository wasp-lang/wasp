module Path.Aliases where

import Path (Path, Abs, Rel, Dir, File)


type RelFile = Path Rel File
type AbsFile = Path Abs File
type RelDir  = Path Rel Dir
type AbsDir  = Path Abs Dir
