{-# LANGUAGE FlexibleInstances #-}

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
    DbConnectionEstablished (DbConnectionEstablished),
    FromOutDir (FromOutDir),
  )
where

import Control.Monad (unless, when)
import Control.Monad.Error.Class (throwError)
import Control.Monad.IO.Class (liftIO)
import Data.Data (Typeable)
import Data.Maybe (fromJust)
import qualified StrongPath as SP
import System.Directory (doesFileExist, doesPathExist, getCurrentDirectory)
import qualified System.FilePath as FP
import Wasp.Cli.Command (CommandError (CommandError), Requirable (checkRequirement), require)
import Wasp.Generator.DbGenerator.Operations (isDbConnectionPossible, testDbConnection)
import Wasp.Project.Common (WaspProjectDir)
import qualified Wasp.Project.Common as Project.Common

-- | Require a Wasp project to exist near the current directory. Get the
-- project directory by pattern matching on the result of 'require':
--
-- @
-- do
--   InWaspProject waspProjectDir <- require
-- @
newtype InWaspProject = InWaspProject (SP.Path' SP.Abs (SP.Dir WaspProjectDir)) deriving (Typeable)

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

data FromOutDir = FromOutDir deriving (Typeable)

-- TODO(carlos): Create a `FromBuildDir` instance of `DbConnectionEstablished`
-- as well. (#2858)
--
-- We can't run our Prisma functions from the `.wasp/build` dir, because some of
-- the files they expect present are not generated as part of `wasp build` (only
-- as part of `wasp start`, which runs in `.wasp/out`). We should refactor our
-- Prisma functions to not depend on these files so we can run them inside
-- `.wasp/build`. Check #2858 for more details.

data DbConnectionEstablished fromDir = DbConnectionEstablished fromDir deriving (Typeable)

instance Requirable (DbConnectionEstablished FromOutDir) where
  checkRequirement = do
    -- NOTE: 'InWaspProject' does not depend on this requirement, so this
    -- call to 'require' will not result in an infinite loop.
    InWaspProject waspProjectDir <- require
    let outDir =
          waspProjectDir
            SP.</> Project.Common.dotWaspDirInWaspProjectDir
            SP.</> Project.Common.generatedCodeDirInDotWaspDir
    dbIsRunning <- liftIO $ isDbConnectionPossible <$> testDbConnection outDir

    if dbIsRunning
      then return $ DbConnectionEstablished FromOutDir
      else throwError noDbError
    where
      noDbError =
        CommandError
          "Can not connect to database"
          ( "The database needs to be running in order to execute this command."
              ++ " You can easily start a managed dev database with `wasp start db`."
          )
