module Wasp.Generator.ServerGenerator.Db.Seed
  ( genDbSeed,
    getPackageJsonPrismaSeedField,
    dbSeedNameEnvVarName,
  )
where

import Data.Aeson (object, (.=))
import qualified Data.Aeson as Aeson
import Data.Functor ((<&>))
import Data.Maybe (maybeToList)
import StrongPath (Dir, File, Path, Path', Posix, Rel, reldirP, relfile, (</>))
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
  return $
    dbSeedTypesFd :
    maybeToList dbSeedFd
  where
    dbSeedTypesFd = C.mkSrcTmplFd [relfile|dbSeed/types.ts|]
    dbSeedFd =
      dbSeedsToTemplateData (getDbSeeds spec) <&> \tmplData ->
        C.mkTmplFdWithData
          (C.srcDirInServerTemplatesDir </> dbSeedScriptInServerTmplSrcDir)
          (Just tmplData)

dbSeedScriptInServerTmplSrcDir :: Path' (Rel C.ServerTemplatesSrcDir) (File ())
dbSeedScriptInServerTmplSrcDir = [relfile|dbSeed.ts|]

pathFromDbSeedScriptToServerSrc :: Path Posix (Rel ()) (Dir C.ServerSrcDir)
pathFromDbSeedScriptToServerSrc = [reldirP|./|]

getPackageJsonPrismaSeedField :: AppSpec -> Maybe String
getPackageJsonPrismaSeedField spec =
  case getDbSeeds spec of
    Just (_ : _) -> Just "npm run db-seed"
    _ -> Nothing

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
