module Wasp.Generator.SdkGenerator.Common
  ( SdkRootDir,
    SdkTemplatesDir,
    sdkRootDirInGeneratedCodeDir,
    sdkRootDirInTemplatesDir,
    extSrcDirInSdkRootDir,
    relDirToRelFileP,
    makeSdkImportPath,
    getOperationTypeName,
    clientTemplatesDirInSdkTemplatesDir,
    serverTemplatesDirInSdkTemplatesDir,
  )
where

import Data.Maybe (fromJust)
import StrongPath
import qualified Wasp.AppSpec.Operation as AS.Operation
import Wasp.Generator.Common (ProjectRootDir)
import Wasp.Generator.ExternalCodeGenerator.Common (GeneratedExternalCodeDir)
import Wasp.Generator.Templates (TemplatesDir)
import Wasp.Util (toUpperFirst)

-- | SDK root directory in a generated Wasp project.
data SdkRootDir

-- | SDK root directory in data files templates directory.
data SdkTemplatesDir

data ClientTemplatesDir

data ServerTemplatesDir

sdkRootDirInGeneratedCodeDir :: Path' (Rel ProjectRootDir) (Dir SdkRootDir)
sdkRootDirInGeneratedCodeDir = [reldir|sdk/wasp|]

sdkRootDirInTemplatesDir :: Path' (Rel TemplatesDir) (Dir SdkTemplatesDir)
sdkRootDirInTemplatesDir = [reldir|sdk/wasp|]

-- | External @src@ directory refers to the user's @src@ directory.
extSrcDirInSdkRootDir :: Path' (Rel SdkRootDir) (Dir GeneratedExternalCodeDir)
extSrcDirInSdkRootDir = [reldir|src|]

relDirToRelFileP :: Path Posix (Rel d) Dir' -> Path Posix (Rel d) File'
relDirToRelFileP path = fromJust $ parseRelFileP $ removeTrailingSlash $ fromRelDirP path
  where
    removeTrailingSlash = reverse . dropWhile (== '/') . reverse

makeSdkImportPath :: Path Posix (Rel SdkRootDir) File' -> Path Posix (Rel s) File'
makeSdkImportPath path = [reldirP|wasp|] </> path

getOperationTypeName :: AS.Operation.Operation -> String
getOperationTypeName operation = toUpperFirst (AS.Operation.getName operation) ++ "_ext"

clientTemplatesDirInSdkTemplatesDir :: Path' (Rel SdkTemplatesDir) (Dir ClientTemplatesDir)
clientTemplatesDirInSdkTemplatesDir = [reldir|client|]

serverTemplatesDirInSdkTemplatesDir :: Path' (Rel SdkTemplatesDir) (Dir ServerTemplatesDir)
serverTemplatesDirInSdkTemplatesDir = [reldir|server|]
