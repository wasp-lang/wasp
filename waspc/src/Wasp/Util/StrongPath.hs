module Wasp.Util.StrongPath
  ( findAllFilesWithSuffix,
    invertRelDir,
  )
where

import Data.List (isSuffixOf)
import Data.Maybe (fromJust)
import qualified StrongPath as SP
import qualified System.FilePath as FP

findAllFilesWithSuffix :: String -> [SP.Path p r (SP.File f)] -> [SP.Path p r (SP.File f)]
findAllFilesWithSuffix extension = filter ((extension `isSuffixOf`) . SP.toFilePath)

-- | Given a relative directory path from @a@ to @b@, returns the inverse
-- path from @b@ back to @a@. Each directory component in the input
-- produces one @..@ segment in the output.
--
-- The path must not contain any @..@ segments, since those can't be inverted.
--
-- >>> invertRelDir [reldir|.|]         -- "./"
-- >>> invertRelDir [reldir|types|]     -- "../"
-- >>> invertRelDir [reldir|.wasp/out|] -- "../../"
invertRelDir :: SP.Path' (SP.Rel a) (SP.Dir b) -> SP.Path' (SP.Rel b) (SP.Dir a)
invertRelDir relDir
  | ".." `elem` pathSegments = error $ "invertRelDir: path contains '..' segment: " ++ SP.fromRelDir relDir
  | otherwise = case pathSegments of
      ["."] -> [SP.reldir|.|]
      _ -> fromJust . SP.parseRelDir $ FP.joinPath $ replicate (length pathSegments) ".."
  where
    pathSegments = FP.splitDirectories . FP.normalise $ SP.fromRelDir relDir
