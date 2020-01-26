module Generator.ExternalCode.Js
       ( generateJsFile
       -- FOR TESTING:
       , resolveJsFileWaspImports
       ) where

import qualified Text.Regex as TR
import Data.Text (Text, pack, unpack)
import Path ((</>))
import qualified Path
import qualified Path.Aliases as Path
import qualified Path.Extra as Path

import qualified Generator.FileDraft as FD
import qualified ExternalCode
import qualified Generator.ExternalCode.Common as Common


generateJsFile :: ExternalCode.File -> FD.FileDraft
generateJsFile file = FD.createTextFileDraft (Common.getExtCodeFileDstPath file) text'
  where
    text = ExternalCode.getFileText file
    text' = resolveJsFileWaspImports jsFilePathInSrcDir text
    jsFilePathInSrcDir = Common.externalCodeDirPathInSrc </>
                         ExternalCode.getFilePathInExtCodeDir file


-- | Takes a file path where the external code js file will be generated, relative to generated src dir.
-- Also takes text of the file. Returns text where special @wasp imports have been replaced with
-- imports that will work.
-- TODO: I had hard time finding popular libraries for more advanced replacements, so for now
--   I just used very simple regex replacement, which might not work in some complicated situations
--   (it will also match on commens and strings and similar).
--   For the future, we should probably use some kind of better regex or even some kind of parser.
--   Possible candidates: replace-attoparsec.
resolveJsFileWaspImports :: Path.RelFile -> Text -> Text
resolveJsFileWaspImports jsFilePathInSrcDir jsFileText = pack $
    -- NOTE(matija): we could not use "\\s+" in the regex below because it does not
    -- work on OS X for some unknown reason. This is why we use " +" instead.
    -- Maybe we should user another regex library, e.g. regexec from
    -- https://hackage.haskell.org/package/regex-posix-0.96.0.0/docs/Text-Regex-Posix-String.html
    TR.subRegex (TR.mkRegex "(from +['\"])@wasp/")
                (unpack jsFileText)
                ("\\1" ++ Path.reversePath (Path.parent jsFilePathInSrcDir) ++ "/")
