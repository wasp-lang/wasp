module Generator.VirtualUserModulesTest where

import StrongPath (File', Path, Posix, Rel, relfileP)
import Test.Hspec (Spec, describe, it, shouldBe)
import qualified Wasp.AppSpec.ExtImport as EI
import Wasp.Generator.SdkGenerator.VirtualUserModules
  ( Runtime (ClientRuntime, ServerRuntime),
    VirtualUserModule (..),
    nubByDeclaredExport,
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
    getTasks = userModule ServerRuntime [relfileP|queries.ts|] (EI.ExtImportField "getTasks")
    getTask = userModule ServerRuntime [relfileP|queries.ts|] (EI.ExtImportField "getTask")
    createTask = userModule ServerRuntime [relfileP|actions.ts|] (EI.ExtImportField "createTask")

spec_nubByDeclaredExport :: Spec
spec_nubByDeclaredExport =
  describe "nubByDeclaredExport" $ do
    it "keeps every export imported from the same user file" $
      importNames (nubByDeclaredExport [getTasks, getTask])
        `shouldBe` [EI.ExtImportField "getTasks", EI.ExtImportField "getTask"]
    it "keeps one module per export registered for both runtimes" $
      importNames (nubByDeclaredExport [clientSchema, serverSchema])
        `shouldBe` [EI.ExtImportField "schema"]
    it "keeps one module per default export, whatever its local name" $
      importNames (nubByDeclaredExport [getOldestTask, oldestTask])
        `shouldBe` [EI.ExtImportModule "getOldestTask"]
  where
    importNames = map (EI.name . extImport)
    getTasks = userModule ServerRuntime [relfileP|queries.ts|] (EI.ExtImportField "getTasks")
    getTask = userModule ServerRuntime [relfileP|queries.ts|] (EI.ExtImportField "getTask")
    clientSchema = userModule ClientRuntime [relfileP|schema.ts|] (EI.ExtImportField "schema")
    serverSchema = userModule ServerRuntime [relfileP|schema.ts|] (EI.ExtImportField "schema")
    getOldestTask = userModule ServerRuntime [relfileP|getOldestTask.ts|] (EI.ExtImportModule "getOldestTask")
    oldestTask = userModule ServerRuntime [relfileP|getOldestTask.ts|] (EI.ExtImportModule "oldestTask")

userModule :: Runtime -> Path Posix (Rel UserSrcDir) File' -> EI.ExtImportName -> VirtualUserModule
userModule runtime' path importName =
  VirtualUserModule
    { runtime = runtime',
      extImport = EI.ExtImport {EI.name = importName, EI.path = path, EI.alias = Nothing},
      registeredTypeModule = [relfileP|server/operations/index|],
      registeredTypeName = "Query"
    }
