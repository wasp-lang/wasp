module Wasp.Cli.Command.Start.ServerRuntimeInputChangeTest where

import qualified StrongPath as SP
import Test.Hspec (Spec, describe, it, shouldBe)
import Wasp.Cli.Command.Compile (CompileResult (..))
import Wasp.Cli.Command.Start.ServerRuntimeInputChange (classifyServerEffect)
import Wasp.Cli.Command.Watch (ProjectFileChange (..), WatchCompileResult (..))
import Wasp.Generator.ServerGenerator.Start (ServerEffect (..))

spec_classifyServerEffect :: Spec
spec_classifyServerEffect =
  describe "classifyServerEffect" $ do
    it "detects watched project src runtime-input extensions" $ do
      classify [ProjectFileChange "src/actions/foo.ts"] [] `shouldBe` RebundleAndRestartServer
      classify [ProjectFileChange "src/actions/foo.mts"] [] `shouldBe` RebundleAndRestartServer
      classify [ProjectFileChange "src/actions/foo.js"] [] `shouldBe` RebundleAndRestartServer
      classify [ProjectFileChange "src/actions/foo.mjs"] [] `shouldBe` RebundleAndRestartServer
      classify [ProjectFileChange "src/actions/foo.json"] [] `shouldBe` RebundleAndRestartServer

    it "ignores project src files that are not server runtime inputs" $ do
      classify [ProjectFileChange "src/styles/foo.css"] [] `shouldBe` NoServerEffect

    it "ignores top-level project changes unless generated server runtime inputs changed" $ do
      classify [ProjectFileChange "main.wasp.ts"] [] `shouldBe` NoServerEffect

    it "detects generated server src runtime-input changes" $ do
      classify [] [Left [SP.relfile|server/src/foo.ts|]] `shouldBe` RebundleAndRestartServer

    it "detects generated server src directory changes" $ do
      classify [] [Right [SP.reldir|server/src/routes|]] `shouldBe` RebundleAndRestartServer

    it "detects generated server env changes" $ do
      classify [] [Left [SP.relfile|server/.env|]] `shouldBe` RestartServer

    it "detects generated server dependency manifest changes" $ do
      classify [] [Left [SP.relfile|server/package.json|]] `shouldBe` RebundleAndRestartServer

    it "uses the strongest effect in a change batch" $ do
      let envChange = Left [SP.relfile|server/.env|]
      let sourceChange = Left [SP.relfile|server/src/foo.ts|]
      classify [] [envChange, sourceChange] `shouldBe` RebundleAndRestartServer
      classify [] [sourceChange, envChange] `shouldBe` RebundleAndRestartServer

    it "does not mix project and generated-app path namespaces" $ do
      classify [ProjectFileChange "server/.env"] [] `shouldBe` NoServerEffect
      classify [] [Left [SP.relfile|src/actions/foo.ts|]] `shouldBe` NoServerEffect
      classify [] [Right [SP.reldir|server/.env|]] `shouldBe` NoServerEffect

    it "ignores generated paths outside server runtime inputs" $ do
      classify [] [Left [SP.relfile|server/README.md|]] `shouldBe` NoServerEffect
      classify [] [Left [SP.relfile|sdk/wasp/server/index.ts|]] `shouldBe` NoServerEffect
  where
    classify projectFileChanges changedGeneratedAppPaths =
      classifyServerEffect $
        WatchCompileResult
          { _watchProjectFileChanges = projectFileChanges,
            _watchCompileResult =
              CompileResult
                { _compileWarnings = [],
                  _compileErrors = [],
                  _compileChangedGeneratedAppPaths = changedGeneratedAppPaths
                }
          }
