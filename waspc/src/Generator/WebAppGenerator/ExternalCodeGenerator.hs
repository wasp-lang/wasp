module Generator.WebAppGenerator.ExternalCodeGenerator
    ( extCodeDirInWebAppSrcDir
    , generatorStrategy
    -- FOR TESTING:
    , resolveJsFileWaspImports
    ) where

import qualified Text.Regex as TR
import Data.Text (Text, pack, unpack)
import qualified Path as P

import StrongPath (Path, Rel, Dir, File, (</>))
import qualified StrongPath as SP
import Path.Extra (reversePath)
import Generator.ExternalCodeGenerator.Common (ExternalCodeGeneratorStrategy(..), GeneratedExternalCodeDir)
import qualified Generator.WebAppGenerator.Common as C

-- | Relative path to directory where external code will be generated.
-- Relative to web app src dir.
extCodeDirInWebAppSrcDir :: Path (Rel C.WebAppSrcDir) (Dir GeneratedExternalCodeDir)
extCodeDirInWebAppSrcDir = SP.fromPathRelDir [P.reldir|ext-src|]

generatorStrategy :: ExternalCodeGeneratorStrategy
generatorStrategy = ExternalCodeGeneratorStrategy
    { _resolveJsFileWaspImports = resolveJsFileWaspImports
    , _extCodeDirInProjectRootDir = C.webAppRootDirInProjectRootDir
                                    </> C.webAppSrcDirInWebAppRootDir
                                    </> extCodeDirInWebAppSrcDir
    }

resolveJsFileWaspImports
    :: Path (Rel GeneratedExternalCodeDir) File -- ^ Path where this JS file will be generated.
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
                ("\\1" ++ reversePath (SP.toPathRelDir $ SP.parent jsFileDstPathInWebAppSrcDir) ++ "/")
  where
    jsFileDstPathInWebAppSrcDir = extCodeDirInWebAppSrcDir </> jsFileDstPathInExtCodeDir
