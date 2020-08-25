module Generator.ExternalCodeGenerator.Js
       ( generateJsFile
       , resolveJsFileWaspImportsForExtCodeDir
       ) where

import qualified Text.Regex as TR
import Data.Text (Text, pack, unpack)

import StrongPath (Path, Rel, File, Dir, (</>))
import qualified StrongPath as SP
import Path.Extra (reversePath)
import qualified Generator.FileDraft as FD
import qualified ExternalCode as EC
import Generator.ExternalCodeGenerator.Common (GeneratedExternalCodeDir)
import qualified Generator.ExternalCodeGenerator.Common as C


generateJsFile :: C.ExternalCodeGeneratorStrategy -> EC.File -> FD.FileDraft
generateJsFile strategy file = FD.createTextFileDraft dstPath text'
  where
    filePathInSrcExtCodeDir = EC.filePathInExtCodeDir file

    filePathInGenExtCodeDir :: Path (Rel C.GeneratedExternalCodeDir) File
    filePathInGenExtCodeDir = C.castRelPathFromSrcToGenExtCodeDir filePathInSrcExtCodeDir

    text = EC.fileText file
    text' = (C._resolveJsFileWaspImports strategy) filePathInGenExtCodeDir text
    dstPath = (C._extCodeDirInProjectRootDir strategy) </> filePathInGenExtCodeDir

-- | Replaces imports that start with "@wasp/" with imports that start from the src dir of the app.
resolveJsFileWaspImportsForExtCodeDir
    :: Path (Rel ()) (Dir GeneratedExternalCodeDir) -- ^ Relative path of ext code dir in src dir of app (web app, server (app), ...)
    -> Path (Rel GeneratedExternalCodeDir) File -- ^ Path where this JS file will be generated.
    -> Text -- ^ Original text of the file.
    -> Text -- ^ Text of the file with special "@wasp" imports resolved (replaced with normal JS imports).
resolveJsFileWaspImportsForExtCodeDir extCodeDirInAppSrcDir jsFileDstPathInExtCodeDir jsFileText = pack $
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
                ("\\1" ++ reversePath (SP.toPathRelDir $ SP.parent jsFileDstPathInAppSrcDir) ++ "/")
  where
    jsFileDstPathInAppSrcDir = extCodeDirInAppSrcDir </> jsFileDstPathInExtCodeDir
