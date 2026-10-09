module Wasp.Generator.ServerGenerator.Db.Seed
  ( genDbSeed,
    dbSeedNameEnvVarName,
    getDbSeeds,
  )
where

import Data.Aeson (object, (.=))
import qualified Data.Aeson as Aeson
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
  return $ case dbSeedsToTemplateData (getDbSeeds spec) of
    Nothing -> []
    Just tmplData ->
      [ C.mkTmplFdWithData (C.srcDirInServerTemplatesDir </> dbSeedScriptInServerTmplSrcDir) (Just tmplData),
        C.mkTmplFd [relfile|rollup.dbSeed.config.js|]
      ]

dbSeedScriptInServerTmplSrcDir :: Path' (Rel C.ServerTemplatesSrcDir) (File ())
dbSeedScriptInServerTmplSrcDir = [relfile|dbSeed.ts|]

pathFromDbSeedScriptToServerSrc :: Path Posix (Rel ()) (Dir C.ServerSrcDir)
pathFromDbSeedScriptToServerSrc = [reldirP|./|]

getDbSeeds :: AppSpec -> Maybe [ExtImport]
getDbSeeds spec = AS.Db.seeds =<< AS.App.db (getApp spec)

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
