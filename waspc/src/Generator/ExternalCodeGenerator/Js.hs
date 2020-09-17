module Generator.ExternalCodeGenerator.Js
       ( generateJsFile
       , resolveJsFileWaspImportsForExtCodeDir
       ) where

import qualified Data.Text as T
import qualified Text.Regex.TDFA as TR
import Data.Text (Text, unpack)

import StrongPath (Path, Rel, File, Dir, (</>))
import qualified StrongPath as SP
import Path.Extra (reversePosixPath, toPosixFilePath)
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
resolveJsFileWaspImportsForExtCodeDir extCodeDirInAppSrcDir jsFileDstPathInExtCodeDir jsFileText =
    let matches = concat (unpack jsFileText TR.=~ ("(from +['\"]@wasp/)" :: String) :: [[String]])
    in foldr replaceFromWasp jsFileText matches
  where
    replaceFromWasp fromWasp = T.replace (T.pack fromWasp) (T.pack $ transformFromWasp fromWasp)
    transformFromWasp fromWasp = (reverse $ drop (length ("@wasp/" :: String)) $ reverse fromWasp) ++ pathPrefix ++ "/"
    pathPrefix = reversePosixPath $ toPosixFilePath $ SP.toPathRelDir $ SP.parent jsFileDstPathInAppSrcDir
    jsFileDstPathInAppSrcDir = extCodeDirInAppSrcDir </> jsFileDstPathInExtCodeDir
