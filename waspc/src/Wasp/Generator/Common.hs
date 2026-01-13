module Wasp.Generator.Common
  ( ProjectRootDir,
    UniversalTemplatesDir,
    universalTemplatesDirInTemplatesDir,
    ServerRootDir,
    WebAppRootDir,
    AppComponentRootDir,
    DbRootDir,
    makeJsonWithEntityData,
    GeneratedSrcDir,
    makeJsArrayFromHaskellList,
    dropExtensionFromImportPath,
    genOptionally,
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
import Wasp.Generator.Monad (Generator)
import Wasp.Generator.Templates (TemplatesDir)
import Wasp.Util (toLowerFirst)

-- | Directory where the whole web app project (client, server, ...) is generated.
data ProjectRootDir

data UniversalTemplatesDir

universalTemplatesDirInTemplatesDir :: Path' (Rel TemplatesDir) (Dir UniversalTemplatesDir)
universalTemplatesDirInTemplatesDir = [reldir|universal|]

-- | Type representing top-level src/ dir in an app component (e.g. in web app or in server).
--   Examples: web-app/src/, server/src/, ... .
class GeneratedSrcDir d

class AppComponentRootDir d

data ServerRootDir

instance AppComponentRootDir ServerRootDir

data WebAppRootDir

instance AppComponentRootDir WebAppRootDir

data DbRootDir

instance AppComponentRootDir DbRootDir

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

genOptionally :: Bool -> a -> Generator [a]
genOptionally True gen = return [gen]
genOptionally False _ = return []
