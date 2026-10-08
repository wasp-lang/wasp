module Generator.VirtualUserModulesTest where

import StrongPath (File', Path, Posix, Rel, relfileP)
import Test.Hspec (Spec, describe, it, shouldBe)
import qualified Wasp.AppSpec.ExtImport as EI
import Wasp.Generator.SdkGenerator.VirtualUserModules
  ( Runtime (ServerRuntime),
    VirtualUserModule (..),
    nubByModuleId,
  )
import Wasp.Project.Common (UserSrcDir)

spec_nubByModuleId :: Spec
spec_nubByModuleId =
  describe "nubByModuleId"
    $ it "keeps one module per user file, however many exports are imported from it"
    $ map (EI.name . extImport) (nubByModuleId [getTasks, createTask, getTask])
      `shouldBe` [EI.ExtImportField "getTasks", EI.ExtImportField "createTask"]
  where
    getTasks = userModule [relfileP|queries.ts|] "getTasks"
    getTask = userModule [relfileP|queries.ts|] "getTask"
    createTask = userModule [relfileP|actions.ts|] "createTask"

    userModule :: Path Posix (Rel UserSrcDir) File' -> String -> VirtualUserModule
    userModule path exportName =
      VirtualUserModule
        { runtime = ServerRuntime,
          extImport = EI.ExtImport {EI.name = EI.ExtImportField exportName, EI.path = path, EI.alias = Nothing},
          registeredTypeModule = [relfileP|server/operations/index|],
          registeredTypeName = "Query"
        }
