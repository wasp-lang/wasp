module Wasp.Generator.Common
  ( GeneratedAppDir,
    UniversalTemplatesDir,
    universalTemplatesDirInTemplatesDir,
    ServerRootDir,
    GeneratedAppComponentDir,
    DbRootDir,
    makeJsonWithEntityData,
    GeneratedAppComponentSrcDir,
    makeJsArrayFromHaskellList,
    dropExtensionFromImportPath,
  )
where

import Data.Aeson (KeyValue ((.=)), object)
import qualified Data.Aeson as Aeson
import Data.List (intercalate)
import Data.Maybe (fromJust)
import StrongPath (Dir, File, Path, Posix, Rel, reldir)
import qualified StrongPath as SP
import StrongPath.Types (Path')
import System.FilePath (splitExtension)
import Wasp.Generator.Templates (TemplatesDir)
import Wasp.Util (toLowerFirst)

-- | Directory where the whole web app project is generated.
data GeneratedAppDir

-- | Directory of a single web app project component in 'GeneratedAppDir'.
class GeneratedAppComponentDir d

-- | Directory of top-level @src/@ dir in 'GeneratedAppComponentDir'.
class GeneratedAppComponentSrcDir d

data ServerRootDir

instance GeneratedAppComponentDir ServerRootDir

data DbRootDir

instance GeneratedAppComponentDir DbRootDir

data UniversalTemplatesDir

universalTemplatesDirInTemplatesDir :: Path' (Rel TemplatesDir) (Dir UniversalTemplatesDir)
universalTemplatesDirInTemplatesDir = [reldir|universal|]

makeJsonWithEntityData :: String -> Aeson.Value
makeJsonWithEntityData name =
  object
    [ "name" .= name,
      "internalTypeName" .= ('_' : name),
      "prismaIdentifier" .= entityNameToPrismaIdentifier name
    ]
  where
    -- Takes a Wasp Entity name (like `SomeTask` from `entity SomeTask {...}`) and
    -- converts it into a corresponding Prisma identifier (e.g., `someTask` used in
    -- `prisma.someTask`).  This is what Prisma implicitly does when translating
    -- `model` declarations to client SDK identifiers. Useful when creating
    -- `context.entities` JS objects in Wasp templates.
    entityNameToPrismaIdentifier :: String -> String
    entityNameToPrismaIdentifier = toLowerFirst

makeJsArrayFromHaskellList :: [String] -> String
makeJsArrayFromHaskellList list = "[" ++ intercalate ", " listOfJsStrings ++ "]"
  where
    listOfJsStrings = map (\s -> "'" ++ s ++ "'") list

dropExtensionFromImportPath :: Path Posix (Rel r) (File f) -> Path Posix (Rel r) (File f)
dropExtensionFromImportPath = fromJust . SP.parseRelFileP . dropExtension . SP.fromRelFileP
  where
    dropExtension = fst . splitExtension
