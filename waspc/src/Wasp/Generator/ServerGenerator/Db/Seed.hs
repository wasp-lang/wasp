module Wasp.Generator.ServerGenerator.Db.Seed
  ( genDbSeed,
    getPackageJsonPrismaSeedField,
    dbSeedNameEnvVarName,
    getDbSeeds,
    areDbSeedsDefined,
    dbSeedViteConfigInServerRootDir,
    dbSeedBundleFromServerRootDir,
  )
where

import Data.Aeson (object, (.=))
import qualified Data.Aeson as Aeson
import Data.Functor ((<&>))
import Data.Maybe (maybeToList)
import StrongPath (Dir, File, File', Path, Path', Posix, Rel, reldirP, relfile, relfileP, (</>))
import Wasp.AppSpec (AppSpec)
import qualified Wasp.AppSpec.App as AS.App
import qualified Wasp.AppSpec.App.Db as AS.Db
import Wasp.AppSpec.ExtImport (ExtImport)
import Wasp.AppSpec.Valid (getApp)
import Wasp.Generator.FileDraft (FileDraft)
import Wasp.Generator.Monad (Generator)
import qualified Wasp.Generator.ServerGenerator.Common as C
import Wasp.Generator.ServerGenerator.JsImport (extImportToImportJson)

genDbSeed :: AppSpec -> Generator [FileDraft]
genDbSeed spec =
  return $ maybeToList dbSeedFd <> maybeToList dbSeedViteConfigFd
  where
    dbSeedFd =
      dbSeedsToTemplateData (getDbSeeds spec) <&> \tmplData ->
        C.mkTmplFdWithData
          (C.srcDirInServerTemplatesDir </> dbSeedScriptInServerTmplSrcDir)
          (Just tmplData)

    -- The config Vite bundles the seeding script with. Only useful when there
    -- is a script to bundle.
    dbSeedViteConfigFd
      | areDbSeedsDefined spec =
          Just $ C.mkTmplFdWithData (C.asTmplFile dbSeedViteConfigInServerRootDir) Nothing
      | otherwise = Nothing

dbSeedScriptInServerTmplSrcDir :: Path' (Rel C.ServerTemplatesSrcDir) (File ())
dbSeedScriptInServerTmplSrcDir = [relfile|dbSeed.ts|]

-- | The Vite config that bundles the seeding script. It sits next to the
-- generated server's `package.json`, which is where `db-seed` runs from.
dbSeedViteConfigInServerRootDir :: Path' (Rel C.ServerRootDir) File'
dbSeedViteConfigInServerRootDir = [relfile|dbSeed.vite.config.js|]

-- | Where the config above writes the bundled seeding script.
--
-- A POSIX path because it ends up in an npm script, which uses forward slashes
-- on every platform.
dbSeedBundleFromServerRootDir :: Path Posix (Rel C.ServerRootDir) File'
dbSeedBundleFromServerRootDir = [relfileP|dist/dbSeed.js|]

pathFromDbSeedScriptToServerSrc :: Path Posix (Rel ()) (Dir C.ServerSrcDir)
pathFromDbSeedScriptToServerSrc = [reldirP|./|]

getPackageJsonPrismaSeedField :: AppSpec -> Maybe String
getPackageJsonPrismaSeedField spec
  | areDbSeedsDefined spec = Just "npm run db-seed"
  | otherwise = Nothing

areDbSeedsDefined :: AppSpec -> Bool
areDbSeedsDefined spec = case getDbSeeds spec of
  Just (_ : _) -> True
  _noSeeds -> False

getDbSeeds :: AppSpec -> Maybe [ExtImport]
getDbSeeds spec = AS.Db.seeds =<< AS.App.db (snd $ getApp spec)

dbSeedsToTemplateData :: Maybe [ExtImport] -> Maybe Aeson.Value
dbSeedsToTemplateData Nothing = Nothing
dbSeedsToTemplateData (Just []) = Nothing
dbSeedsToTemplateData (Just seeds) =
  Just $
    object
      [ "dbSeeds" .= (dbSeedToTemplateData <$> seeds),
        "dbSeedNameEnvVarName" .= dbSeedNameEnvVarName
      ]
  where
    dbSeedToTemplateData :: ExtImport -> Aeson.Value
    dbSeedToTemplateData extImport =
      extImportToImportJson pathFromDbSeedScriptToServerSrc (Just extImport)

dbSeedNameEnvVarName :: String
dbSeedNameEnvVarName = "WASP_DB_SEED_NAME"
