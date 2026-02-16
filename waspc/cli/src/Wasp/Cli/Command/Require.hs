module Wasp.Cli.Command.Require
  ( -- * Asserting Requirements

    -- There are some requirements we want to assert in command code, such as
    -- ensuring the command is being run inside a wasp project directory. We
    -- might end up wanting to check each requirement multiple times, especially
    -- if we want the value from it (like getting the wasp project directory),
    -- but we also want to avoid duplicating work. Using 'require' results in
    -- checked requirements being stored so they can be immediately retrieved
    -- when checking the same requirements additional times.
    --
    -- For example, you can check if you are in a wasp project by doing
    --
    -- @
    -- do
    --   InWaspProject waspProjectDir <- require
    -- @
    --
    -- See instances of 'Requirable' for what kinds of requirements are
    -- supported. To implement a new requirable type, give your type an instance
    -- of 'Requirable'.
    require,

    -- * Requirables
    Requirable (checkRequirement),
    InWaspProject (InWaspProject),
    GeneratedCodeIsProduction (GeneratedCodeIsProduction),
    GeneratedCodeIsDevelopment (GeneratedCodeIsDevelopment),
    DbConnectionEstablished (DbConnectionEstablished),
  )
where

import Control.Monad (unless, when)
import Control.Monad.Error.Class (throwError)
import Control.Monad.IO.Class (liftIO)
import Data.Bool (bool)
import Data.Data (Typeable)
import Data.Maybe (fromJust)
import StrongPath (Abs, Dir, Path')
import qualified StrongPath as SP
import System.Directory (doesFileExist, doesPathExist, getCurrentDirectory)
import qualified System.FilePath as FP
import Wasp.Cli.Command (Command, CommandError (CommandError), Requirable (checkRequirement), require)
import Wasp.Generator.Common (ProjectRootDir)
import Wasp.Generator.DbGenerator.Operations (isDbConnectionPossible, testDbConnection)
import qualified Wasp.Generator.WaspInfo as WaspInfo
import qualified Wasp.Project.BuildType as BuildType
import Wasp.Project.Common (WaspProjectDir)
import qualified Wasp.Project.Common as Project.Common

-- | Require a Wasp project to exist near the current directory. Get the
-- project directory by pattern matching on the result of 'require':
--
-- @
-- do
--   InWaspProject waspProjectDir <- require
-- @
newtype InWaspProject = InWaspProject (Path' Abs (Dir WaspProjectDir)) deriving (Typeable)

instance Requirable InWaspProject where
  checkRequirement = do
    -- Recursively searches up from CWD until @.wasproot@ file is found, or
    -- throw an error if it is never found.
    currentDir <- fromJust . SP.parseAbsDir <$> liftIO getCurrentDirectory
    findWaspProjectRoot currentDir
    where
      findWaspProjectRoot currentDir = do
        let absCurrentDirFp = SP.fromAbsDir currentDir
        doesCurrentDirExist <- liftIO $ doesPathExist absCurrentDirFp
        unless doesCurrentDirExist (throwError notFoundError)
        let dotWaspRootFilePath = absCurrentDirFp FP.</> SP.fromRelFile Project.Common.dotWaspRootFileInWaspProjectDir
        isCurrentDirRoot <- liftIO $ doesFileExist dotWaspRootFilePath
        if isCurrentDirRoot
          then return $ InWaspProject $ SP.castDir currentDir
          else do
            let parentDir = SP.parent currentDir
            when (parentDir == currentDir) (throwError notFoundError)
            findWaspProjectRoot parentDir

      notFoundError =
        CommandError
          "Wasp command failed"
          ( "Couldn't find wasp project root - make sure"
              ++ " you are running this command from a Wasp project."
          )

data DbConnectionEstablished = DbConnectionEstablished deriving (Typeable)

instance Requirable DbConnectionEstablished where
  checkRequirement = do
    -- TODO: Remove `GeneratedCodeIsDevelopment` requirement:
    -- https://github.com/wasp-lang/wasp/issues/2858.
    -- The reason why we need it is because a Production build by design does
    -- not have some files like `.env` or `prisma.schema`, which makes it tricky
    -- to determine the database location. See the linked issue for more
    -- details.
    GeneratedCodeIsDevelopment outDir <- require

    dbIsRunning <- liftIO $ isDbConnectionPossible <$> testDbConnection outDir

    if dbIsRunning
      then return DbConnectionEstablished
      else throwError noDbError
    where
      noDbError =
        CommandError
          "Can not connect to database"
          ( "The database needs to be running in order to execute this command."
              ++ " You can easily start a managed dev database with `wasp start db`."
          )

data GeneratedCodeIsDevelopment
  = GeneratedCodeIsDevelopment (Path' Abs (Dir ProjectRootDir))
  deriving (Typeable)

instance Requirable GeneratedCodeIsDevelopment where
  checkRequirement =
    isBuildTypeCompatibleWithGeneratedCode BuildType.Development
      >>= maybe
        (throwError noDevelopmentCodeError)
        (return . GeneratedCodeIsDevelopment)
    where
      noDevelopmentCodeError =
        CommandError
          "Built app does not exist"
          "You can build the app with the `wasp start` or `wasp compile` commands."

data GeneratedCodeIsProduction
  = GeneratedCodeIsProduction (Path' Abs (Dir ProjectRootDir))
  deriving (Typeable)

instance Requirable GeneratedCodeIsProduction where
  checkRequirement =
    isBuildTypeCompatibleWithGeneratedCode BuildType.Production
      >>= maybe
        (throwError noProductionCodeError)
        (return . GeneratedCodeIsProduction)
    where
      noProductionCodeError =
        CommandError
          "Built app does not exist"
          "You can build the app with the `wasp build` command."

isBuildTypeCompatibleWithGeneratedCode ::
  BuildType.BuildType ->
  Command (Maybe (Path' Abs (Dir ProjectRootDir)))
isBuildTypeCompatibleWithGeneratedCode expectedBuildType = do
  InWaspProject waspProjectDir <- require

  let generatedCodeDir =
        waspProjectDir
          SP.</> Project.Common.dotWaspDirInWaspProjectDir
          SP.</> Project.Common.generatedCodeDirInDotWaspDir

  liftIO $
    bool Nothing (Just generatedCodeDir)
      <$> expectedBuildType `WaspInfo.isCompatibleWithExistingBuildAt` generatedCodeDir
