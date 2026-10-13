module Wasp.Cli.Command.Start.ServerRuntimeInputChangeTest where

import qualified StrongPath as SP
import Test.Hspec (Spec, describe, it, shouldBe)
import Wasp.Cli.Command.Start.ServerRuntimeInputChange (classifyServerEffect)
import Wasp.Cli.Command.Watch (ProjectFileChange (..))
import Wasp.Generator.ServerGenerator.Start (ServerEffect (..))

spec_classifyServerEffect :: Spec
spec_classifyServerEffect =
  describe "classifyServerEffect" $ do
    it "detects watched project src runtime-input extensions" $ do
      classifyServerEffect [ProjectFileChange "src/actions/foo.ts"] [] `shouldBe` RebundleAndRestartServer
      classifyServerEffect [ProjectFileChange "src/actions/foo.mts"] [] `shouldBe` RebundleAndRestartServer
      classifyServerEffect [ProjectFileChange "src/actions/foo.js"] [] `shouldBe` RebundleAndRestartServer
      classifyServerEffect [ProjectFileChange "src/actions/foo.mjs"] [] `shouldBe` RebundleAndRestartServer
      classifyServerEffect [ProjectFileChange "src/actions/foo.json"] [] `shouldBe` RebundleAndRestartServer

    it "ignores project src files that are not server runtime inputs" $ do
      classifyServerEffect [ProjectFileChange "src/styles/foo.css"] [] `shouldBe` NoServerEffect

    it "ignores top-level project changes unless generated server runtime inputs changed" $ do
      classifyServerEffect [ProjectFileChange "main.wasp.ts"] [] `shouldBe` NoServerEffect

    it "detects generated server src runtime-input changes" $ do
      classifyServerEffect [] [Left [SP.relfile|server/src/foo.ts|]] `shouldBe` RebundleAndRestartServer

    it "detects generated server src directory changes" $ do
      classifyServerEffect [] [Right [SP.reldir|server/src/routes|]] `shouldBe` RebundleAndRestartServer

    it "detects generated server env changes" $ do
      classifyServerEffect [] [Left [SP.relfile|server/.env|]] `shouldBe` RestartServer

    it "detects generated server dependency manifest changes" $ do
      classifyServerEffect [] [Left [SP.relfile|server/package.json|]] `shouldBe` RebundleAndRestartServer

    it "uses the strongest effect in a change batch" $ do
      let envChange = Left [SP.relfile|server/.env|]
      let sourceChange = Left [SP.relfile|server/src/foo.ts|]
      classifyServerEffect [] [envChange, sourceChange] `shouldBe` RebundleAndRestartServer
      classifyServerEffect [] [sourceChange, envChange] `shouldBe` RebundleAndRestartServer

    it "does not mix project and generated-app path namespaces" $ do
      classifyServerEffect [ProjectFileChange "server/.env"] [] `shouldBe` NoServerEffect
      classifyServerEffect [] [Left [SP.relfile|src/actions/foo.ts|]] `shouldBe` NoServerEffect
      classifyServerEffect [] [Right [SP.reldir|server/.env|]] `shouldBe` NoServerEffect

    it "rebundles when email configuration changes only generated SDK code" $ do
      classifyServerEffect
        [ProjectFileChange "main.wasp.ts"]
        [Left [SP.relfile|sdk/wasp/server/email/core/helpers.ts|]]
        `shouldBe` RebundleAndRestartServer

    it "detects generated server SDK directory changes" $ do
      classifyServerEffect [] [Right [SP.reldir|sdk/wasp/server/email|]]
        `shouldBe` RebundleAndRestartServer

    it "detects generated SDK changes outside the SDK's client code" $ do
      classifyServerEffect [] [Left [SP.relfile|sdk/wasp/auth/session.ts|]] `shouldBe` RebundleAndRestartServer
      classifyServerEffect [] [Left [SP.relfile|sdk/wasp/core/serialization/index.ts|]] `shouldBe` RebundleAndRestartServer
      classifyServerEffect [] [Left [SP.relfile|sdk/wasp/entities/index.ts|]] `shouldBe` RebundleAndRestartServer
      classifyServerEffect [] [Right [SP.reldir|sdk/wasp/auth|]] `shouldBe` RebundleAndRestartServer

    it "ignores generated SDK client code" $ do
      classifyServerEffect [] [Right [SP.reldir|sdk/wasp/client|]] `shouldBe` NoServerEffect
      classifyServerEffect [] [Right [SP.reldir|sdk/wasp/client/router|]] `shouldBe` NoServerEffect

    it "restarts the server when the Prisma schema changes" $ do
      classifyServerEffect [] [Left [SP.relfile|db/schema.prisma|]] `shouldBe` RestartServer

    it "ignores generated paths outside server runtime inputs" $ do
      classifyServerEffect [] [Left [SP.relfile|server/README.md|]] `shouldBe` NoServerEffect
      classifyServerEffect [] [Left [SP.relfile|sdk/wasp/client/index.ts|]] `shouldBe` NoServerEffect
