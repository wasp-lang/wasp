module Wasp.Generator.ExternalCodeGenerator.Js
  ( generateJsFile,
    resolveJsFileWaspImportsForExtCodeDir,
  )
where

import Data.Maybe (fromJust)
import Data.Text (Text, unpack)
import qualified Data.Text as T
import FilePath.Extra (reversePosixPath)
import StrongPath (Dir, File', Path', Rel, (</>))
import qualified StrongPath as SP
import qualified Text.Regex.TDFA as TR
import qualified Wasp.AppSpec.ExternalCode as EC
import Wasp.Generator.ExternalCodeGenerator.Common (GeneratedExternalCodeDir)
import qualified Wasp.Generator.ExternalCodeGenerator.Common as C
import qualified Wasp.Generator.FileDraft as FD
import Wasp.Generator.Monad (Generator)

generateJsFile :: C.ExternalCodeGeneratorStrategy -> EC.File -> Generator FD.FileDraft
generateJsFile strategy file = return $ FD.createTextFileDraft dstPath text'
  where
    filePathInSrcExtCodeDir = EC.filePathInExtCodeDir file

    filePathInGenExtCodeDir :: Path' (Rel C.GeneratedExternalCodeDir) File'
    filePathInGenExtCodeDir = C.castRelPathFromSrcToGenExtCodeDir filePathInSrcExtCodeDir

    text = EC.fileText file
    text' = C._resolveJsFileWaspImports strategy filePathInGenExtCodeDir text
    dstPath = C._extCodeDirInProjectRootDir strategy </> filePathInGenExtCodeDir

-- | Replaces imports that start with "@wasp/" with imports that start from the src dir of the app.
resolveJsFileWaspImportsForExtCodeDir ::
  -- | Relative path of ext code dir in src dir of app (web app, server (app), ...)
  Path' (Rel ()) (Dir GeneratedExternalCodeDir) ->
  -- | Path where this JS file will be generated.
  Path' (Rel GeneratedExternalCodeDir) File' ->
  -- | Original text of the file.
  Text ->
  -- | Text of the file with special "@wasp" imports resolved (replaced with normal JS imports).
  Text
resolveJsFileWaspImportsForExtCodeDir extCodeDirInAppSrcDir jsFileDstPathInExtCodeDir jsFileText =
  let matches = concat (unpack jsFileText TR.=~ ("(from +['\"]@wasp/)" :: String) :: [[String]])
   in foldr replaceFromWasp jsFileText matches
  where
    replaceFromWasp fromWasp = T.replace (T.pack fromWasp) (T.pack $ transformFromWasp fromWasp)
    transformFromWasp fromWasp = reverse (drop (length ("@wasp/" :: String)) $ reverse fromWasp) ++ pathPrefix ++ "/"
    pathPrefix = reversePosixPath $ SP.fromRelDirP $ fromJust $ SP.relDirToPosix $ SP.parent jsFileDstPathInAppSrcDir
    jsFileDstPathInAppSrcDir = extCodeDirInAppSrcDir </> jsFileDstPathInExtCodeDir
