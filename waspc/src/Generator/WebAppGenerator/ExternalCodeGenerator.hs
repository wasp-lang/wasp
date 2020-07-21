module Generator.WebAppGenerator.ExternalCodeGenerator
    ( extCodeDirInWebAppSrcDir
    , generatorStrategy
    -- FOR TESTING:
    , resolveJsFileWaspImports
    ) where

import qualified Text.Regex as TR
import Data.Text (Text, pack, unpack)
import Path (reldir)
import qualified Path
import qualified Path.Aliases as Path
import qualified Path.Extra as Path
import Generator.ExternalCodeGenerator.Common (ExternalCodeGeneratorStrategy(..))
import qualified Generator.WebAppGenerator.Common as C

-- | Relative path to directory where external code will be generated.
-- Relative to web app src dir.
extCodeDirInWebAppSrcDir :: Path.RelDir
extCodeDirInWebAppSrcDir = [reldir|ext-src|]

generatorStrategy :: ExternalCodeGeneratorStrategy
generatorStrategy = ExternalCodeGeneratorStrategy
    { _resolveJsFileWaspImports = resolveJsFileWaspImports
    , _extCodeDirInProjectRootDir = C.webAppRootDirInProjectRootDir
                                    Path.</> C.webAppSrcDirInWebAppRootDir
                                    Path.</> extCodeDirInWebAppSrcDir
    }

resolveJsFileWaspImports
    -- | Path where this JS file will be generated, relative to the generated external code dir.
    :: Path.RelFile
    -> Text -- ^ Original text of the file.
    -> Text -- ^ Text of the file with special "@wasp" imports resolved (replaced with normal JS imports).
resolveJsFileWaspImports jsFileDstPathInExtCodeDir jsFileText = pack $
    -- TODO(martin): I had hard time finding popular libraries for more advanced replacements, so for now
    --   I just used very simple regex replacement, which might not work in some complicated situations
    --   (it will also match on commens and strings and similar).
    --   For the future, we should probably use some kind of better regex or even some kind of parser.
    --   Possible candidates: replace-attoparsec.
    -- NOTE(matija): we could not use "\\s+" in the regex below because it does not
    -- work on OS X for some unknown reason. This is why we use " +" instead.
    -- Maybe we should user another regex library, e.g. regexec from
    -- https://hackage.haskell.org/package/regex-posix-0.96.0.0/docs/Text-Regex-Posix-String.html
    TR.subRegex (TR.mkRegex "(from +['\"])@wasp/")
                (unpack jsFileText)
                ("\\1" ++ Path.reversePath (Path.parent jsFileDstPathInWebAppSrcDir) ++ "/")
  where
    jsFileDstPathInWebAppSrcDir = extCodeDirInWebAppSrcDir Path.</> jsFileDstPathInExtCodeDir
