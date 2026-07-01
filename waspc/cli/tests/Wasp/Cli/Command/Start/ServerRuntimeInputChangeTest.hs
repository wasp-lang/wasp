module Wasp.Cli.Command.Start.ServerRuntimeInputChangeTest where

import qualified StrongPath as SP
import Test.Hspec (Spec, describe, it, shouldBe)
import Wasp.Cli.Command.Compile (CompileResult (..))
import Wasp.Cli.Command.Start.ServerRuntimeInputChange (classifyServerRuntimeInputChange)
import Wasp.Cli.Command.Watch (ProjectFileChange (..), WatchCompileResult (..))
import Wasp.Generator.ServerGenerator.ServerProcessSupervisor (ServerRuntimeInputChange (..))
import Wasp.Generator.WriteFileDrafts (GeneratedAppPathChange (..))

spec_classifyServerRuntimeInputChange :: Spec
spec_classifyServerRuntimeInputChange =
  describe "classifyServerRuntimeInputChange" $ do
    it "detects watched project src runtime-input extensions" $ do
      classify [ProjectFileChange "src/actions/foo.ts"] [] `shouldBe` ServerRuntimeInputMightHaveChanged
      classify [ProjectFileChange "src/actions/foo.mts"] [] `shouldBe` ServerRuntimeInputMightHaveChanged
      classify [ProjectFileChange "src/actions/foo.js"] [] `shouldBe` ServerRuntimeInputMightHaveChanged
      classify [ProjectFileChange "src/actions/foo.mjs"] [] `shouldBe` ServerRuntimeInputMightHaveChanged
      classify [ProjectFileChange "src/actions/foo.json"] [] `shouldBe` ServerRuntimeInputMightHaveChanged

    it "ignores project src files that are not server runtime inputs" $ do
      classify [ProjectFileChange "src/styles/foo.css"] [] `shouldBe` NoServerRuntimeInputChange

    it "ignores top-level project changes unless generated server runtime inputs changed" $ do
      classify [ProjectFileChange "main.wasp.ts"] [] `shouldBe` NoServerRuntimeInputChange

    it "detects generated server src runtime-input writes and deletes" $ do
      classify [] [GeneratedAppPathWritten $ Left [SP.relfile|server/src/foo.ts|]] `shouldBe` ServerRuntimeInputMightHaveChanged
      classify [] [GeneratedAppPathDeleted $ Left [SP.relfile|server/src/foo.ts|]] `shouldBe` ServerRuntimeInputMightHaveChanged

    it "detects generated server src directory deletes" $ do
      classify [] [GeneratedAppPathDeleted $ Right [SP.reldir|server/src/routes|]] `shouldBe` ServerRuntimeInputMightHaveChanged

    it "detects generated server env writes and deletes" $ do
      classify [] [GeneratedAppPathWritten $ Left [SP.relfile|server/.env|]] `shouldBe` ServerRuntimeInputMightHaveChanged
      classify [] [GeneratedAppPathDeleted $ Left [SP.relfile|server/.env|]] `shouldBe` ServerRuntimeInputMightHaveChanged

    it "ignores generated paths outside server runtime inputs" $ do
      classify [] [GeneratedAppPathWritten $ Left [SP.relfile|server/package.json|]] `shouldBe` NoServerRuntimeInputChange
      classify [] [GeneratedAppPathWritten $ Left [SP.relfile|sdk/wasp/server/index.ts|]] `shouldBe` NoServerRuntimeInputChange
  where
    classify projectFileChanges generatedAppPathChanges =
      classifyServerRuntimeInputChange $
        WatchCompileResult
          { _watchProjectFileChanges = projectFileChanges,
            _watchCompileResult =
              CompileResult
                { _compileWarnings = [],
                  _compileErrors = [],
                  _compileGeneratedAppPathChanges = generatedAppPathChanges
                }
          }
