module Wasp.Generator.ServerGenerator.TaskRunner
  ( TaskRunner (..),
    genTaskRunners,
    hasTaskRunner,
    taskRunnerRollupConfigInServerRootDir,
    taskRunnerBundleInServerRootDir,
  )
where

import Data.Aeson (object, (.=))
import qualified Data.Aeson as Aeson
import Data.Maybe (fromMaybe)
import StrongPath (Dir, File', Path, Path', Posix, Rel, reldirP, relfile, (</>))
import Wasp.AppSpec (AppSpec)
import qualified Wasp.AppSpec as AS
import qualified Wasp.AppSpec.App as AS.App
import qualified Wasp.AppSpec.App.Db as AS.Db
import qualified Wasp.AppSpec.ExtImport as AS.ExtImport
import qualified Wasp.AppSpec.Task as AS.Task
import Wasp.AppSpec.Valid (getApp)
import Wasp.Generator.Common (makeJsonWithEntityData)
import Wasp.Generator.FileDraft (FileDraft)
import Wasp.Generator.Monad (Generator)
import qualified Wasp.Generator.ServerGenerator.Common as C
import Wasp.Generator.ServerGenerator.JsImport (getAliasedJsImportStmtAndIdentifier)

-- | A server script that runs one of a set of named functions, picked by its
-- first command-line argument, and passes it the remaining arguments.
data TaskRunner
  = -- | Runs the app's tasks, for `wasp task run` and the `task` npm script.
    AppTasks
  | -- | Runs the app's db seeds, for `wasp db seed`.
    DbSeeds
  deriving (Show, Eq, Enum, Bounded)

-- | Whether the app defines anything for the runner to run. If it doesn't, we
-- don't generate the runner.
hasTaskRunner :: AppSpec -> TaskRunner -> Bool
hasTaskRunner spec AppTasks = not . null $ AS.getTasks spec
hasTaskRunner spec DbSeeds = not . null $ getDbSeeds spec

genTaskRunners :: AppSpec -> Generator [FileDraft]
genTaskRunners spec =
  return $
    concatMap genTaskRunner runners
      ++ [C.mkSrcTmplFd [relfile|runTaskFromCli.ts|] | not (null runners)]
  where
    runners = filter (hasTaskRunner spec) [minBound .. maxBound]

    genTaskRunner runner =
      [ C.mkTmplFdWithData (C.srcDirInServerTemplatesDir </> scriptInServerTemplatesSrcDir runner) (Just $ tmplData runner),
        C.mkTmplFd $ C.asTmplFile $ taskRunnerRollupConfigInServerRootDir runner
      ]

    tmplData AppTasks = object ["tasks" .= map taskTmplData (AS.getTasks spec)]
    tmplData DbSeeds = object ["dbSeeds" .= map dbSeedTmplData (getDbSeeds spec)]

taskTmplData :: AS.Task.Task -> Aeson.Value
taskTmplData task =
  object $
    userFnImportTmplData task.name task.fn
      ++ [ "taskName" .= task.name,
           "entities" .= maybe [] (map (makeJsonWithEntityData . AS.refName)) task.entities
         ]

dbSeedTmplData :: AS.ExtImport.ExtImport -> Aeson.Value
dbSeedTmplData seed =
  object $
    userFnImportTmplData seedName seed
      ++ ["seedName" .= seedName]
  where
    seedName = AS.ExtImport.importIdentifier seed

-- | We alias the user's imports so that their names can't clash with the
-- runner's own imports (e.g. a task named @prisma@).
userFnImportTmplData :: String -> AS.ExtImport.ExtImport -> [(Aeson.Key, Aeson.Value)]
userFnImportTmplData name extImport =
  [ "importStatement" .= importStatement,
    "importIdentifier" .= importIdentifier
  ]
  where
    (importStatement, importIdentifier) =
      getAliasedJsImportStmtAndIdentifier ("_waspUserFn_" <> name) pathFromRunnerScriptToServerSrc extImport

getDbSeeds :: AppSpec -> [AS.ExtImport.ExtImport]
getDbSeeds spec = fromMaybe [] $ AS.Db.seeds =<< AS.App.db (getApp spec)

scriptInServerTemplatesSrcDir :: TaskRunner -> Path' (Rel C.ServerTemplatesSrcDir) File'
scriptInServerTemplatesSrcDir AppTasks = [relfile|tasks.ts|]
scriptInServerTemplatesSrcDir DbSeeds = [relfile|dbSeed.ts|]

pathFromRunnerScriptToServerSrc :: Path Posix (Rel ()) (Dir C.ServerSrcDir)
pathFromRunnerScriptToServerSrc = [reldirP|./|]

taskRunnerRollupConfigInServerRootDir :: TaskRunner -> Path' (Rel C.ServerRootDir) File'
taskRunnerRollupConfigInServerRootDir AppTasks = [relfile|rollup.tasks.config.js|]
taskRunnerRollupConfigInServerRootDir DbSeeds = [relfile|rollup.dbSeed.config.js|]

taskRunnerBundleInServerRootDir :: TaskRunner -> Path' (Rel C.ServerRootDir) File'
taskRunnerBundleInServerRootDir AppTasks = [relfile|bundle/tasks.js|]
taskRunnerBundleInServerRootDir DbSeeds = [relfile|bundle/dbSeed.js|]
