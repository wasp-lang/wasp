module Wasp.Cli.Command.Require.GeneratedApp
  ( GeneratedAppIsDevelopment (GeneratedAppIsDevelopment),
    GeneratedAppIsProduction (GeneratedAppIsProduction),
  )
where

import Control.Monad.Error.Class (throwError)
import Control.Monad.IO.Class (liftIO)
import Data.Bool (bool)
import Data.Data (Typeable)
import StrongPath (Abs, Dir, Path')
import qualified StrongPath as SP
import Wasp.Cli.Command (Command, CommandError (CommandError), Requirable (checkRequirement), require)
import Wasp.Cli.Command.Require.InWaspProject (InWaspProject (InWaspProject))
import Wasp.Generator.Common (GeneratedAppDir)
import qualified Wasp.Generator.WaspInfo as WaspInfo
import qualified Wasp.Project.BuildType as BuildType
import qualified Wasp.Project.Common as Project.Common

data GeneratedAppIsDevelopment
  = GeneratedAppIsDevelopment (Path' Abs (Dir GeneratedAppDir))
  deriving (Typeable)

instance Requirable GeneratedAppIsDevelopment where
  checkRequirement =
    isBuildTypeCompatibleWithGeneratedApp BuildType.Development
      >>= maybe
        (throwError noDevelopmentCodeError)
        (return . GeneratedAppIsDevelopment)
    where
      noDevelopmentCodeError =
        CommandError
          "Built app does not exist"
          "You can build the app with the `wasp start` or `wasp compile` commands."

data GeneratedAppIsProduction
  = GeneratedAppIsProduction (Path' Abs (Dir GeneratedAppDir))
  deriving (Typeable)

instance Requirable GeneratedAppIsProduction where
  checkRequirement =
    isBuildTypeCompatibleWithGeneratedApp BuildType.Production
      >>= maybe
        (throwError noProductionCodeError)
        (return . GeneratedAppIsProduction)
    where
      noProductionCodeError =
        CommandError
          "Built app does not exist"
          "You can build the app with the `wasp build` command."

isBuildTypeCompatibleWithGeneratedApp ::
  BuildType.BuildType ->
  Command (Maybe (Path' Abs (Dir GeneratedAppDir)))
isBuildTypeCompatibleWithGeneratedApp expectedBuildType = do
  InWaspProject waspProjectDir <- require

  let generatedAppDir =
        waspProjectDir
          SP.</> Project.Common.dotWaspDirInWaspProjectDir
          SP.</> Project.Common.generatedAppDirInDotWaspDir

  liftIO $
    bool Nothing (Just generatedAppDir)
      <$> expectedBuildType `WaspInfo.isCompatibleWithExistingBuildAt` generatedAppDir
