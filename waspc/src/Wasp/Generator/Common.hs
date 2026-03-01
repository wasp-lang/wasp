module Wasp.Generator.Common
  ( ProjectRootDir,
    UniversalTemplatesDir,
    universalTemplatesDirInTemplatesDir,
    ServerRootDir,
    AppComponentRootDir,
    DbRootDir,
    makeJsonWithEntityData,
    makeJsonWithAliasEntityData,
    GeneratedSrcDir,
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

data DbRootDir

instance AppComponentRootDir DbRootDir

makeJsonWithEntityData :: String -> Aeson.Value
makeJsonWithEntityData name =
  object
    [ "name" .= name,
      "entityTypeName" .= name,
      "internalTypeName" .= ('_' : name),
      "prismaIdentifier" .= entityNameToPrismaIdentifier name
    ]

-- | Like 'makeJsonWithEntityData' but for entity aliases from module entity maps.
-- The alias name is used as the entity identifier, while the real entity type name
-- is used for the underlying Prisma type and delegate.
makeJsonWithAliasEntityData ::
  -- | Alias name (e.g. "Todo")
  String ->
  -- | Real entity name (e.g. "TodoItem")
  String ->
  Aeson.Value
makeJsonWithAliasEntityData aliasName realEntityName =
  object
    [ "name" .= aliasName,
      "entityTypeName" .= realEntityName,
      "internalTypeName" .= ('_' : aliasName),
      "prismaIdentifier" .= entityNameToPrismaIdentifier realEntityName
    ]

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
