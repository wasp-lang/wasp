module Wasp.Generator.Common
  ( ProjectRootDir,
    ServerRootDir,
    WebAppRootDir,
    ModuleRootDir,
    DbRootDir,
    latestMajorNodeVersion,
    nodeVersionRange,
    npmVersionRange,
    prismaVersion,
    makeJsonWithEntityData,
    entityNameToPrismaIdentifier,
  )
where

import Data.Aeson (KeyValue ((.=)), object)
import qualified Data.Aeson as Aeson
import Data.Char (toLower)
import qualified Wasp.SemanticVersion as SV

-- | Directory where the whole web app project (client, server, ...) is generated.
data ProjectRootDir

class ModuleRootDir d

data ServerRootDir

instance ModuleRootDir ServerRootDir

data WebAppRootDir

instance ModuleRootDir WebAppRootDir

data DbRootDir

instance ModuleRootDir DbRootDir

-- | Latest concrete major node version supported by the nodeVersionRange, and
--   therefore by Wasp.
--   Here we assume that nodeVersionRange is using latestNodeLTSVersion as its basis.
--   TODO: instead of making assumptions, extract the latest major node version
--   directly from the nodeVersionRange.
latestMajorNodeVersion :: SV.Version
latestMajorNodeVersion = latestNodeLTSVersion

nodeVersionRange :: SV.Range
nodeVersionRange = SV.Range [SV.backwardsCompatibleWith latestNodeLTSVersion]

latestNodeLTSVersion :: SV.Version
latestNodeLTSVersion = SV.Version 18 12 0

-- | Range of npm versions that Wasp and generated projects work correctly with.
npmVersionRange :: SV.Range
npmVersionRange = SV.Range [SV.backwardsCompatibleWith latestLTSVersion]
  where
    latestLTSVersion = SV.Version 8 19 2 -- Goes with node 18 (but also higher versions too).

prismaVersion :: SV.Version
prismaVersion = SV.Version 4 5 0

makeJsonWithEntityData :: String -> Aeson.Value
makeJsonWithEntityData name =
  object
    [ "name" .= name,
      "internalTypeName" .= ('_' : name),
      "prismaIdentifier" .= entityNameToPrismaIdentifier name
    ]

-- | Takes a Wasp Entity name (like `SomeTask` from `entity SomeTask {...}`) and
-- converts it into a corresponding Prisma identifier (e.g., `someTask` used in
-- `prisma.someTask`).  This is what Prisma implicitly does when translating
-- `model` declarations to client SDK identifiers. Useful when creating
-- `context.entities` JS objects in Wasp templates.
entityNameToPrismaIdentifier :: String -> String
entityNameToPrismaIdentifier entityName = toLower (head entityName) : tail entityName
