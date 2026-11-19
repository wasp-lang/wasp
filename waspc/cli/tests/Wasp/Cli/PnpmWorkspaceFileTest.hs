module Wasp.Cli.PnpmWorkspaceFileTest where

import Test.Hspec
import Wasp.Cli.PnpmWorkspaceFile (appendOverrides, removeTopLevelKey)

spec_removeTopLevelKey :: Spec
spec_removeTopLevelKey = do
  it "drops the key with its block and keeps the rest" $ do
    removeTopLevelKey
      "packages"
      ( unlines
          [ "packages:",
            "  - packages/*",
            "  # Examples.",
            "  - examples/*",
            "",
            "allowBuilds:",
            "  prisma: true"
          ]
      )
      `shouldBe` unlines ["allowBuilds:", "  prisma: true"]

  it "drops a key whose value is on the same line" $ do
    removeTopLevelKey "minimumReleaseAge" (unlines ["minimumReleaseAge: 10080", "autoInstallPeers: true"])
      `shouldBe` unlines ["autoInstallPeers: true"]

  it "leaves a file without the key as it is" $ do
    removeTopLevelKey "packages" (unlines ["allowBuilds:", "  prisma: true"])
      `shouldBe` unlines ["allowBuilds:", "  prisma: true"]

spec_appendOverrides :: Spec
spec_appendOverrides = do
  it "adds the overrides after the existing settings" $ do
    appendOverrides [("@wasp/cli", "file:dev-packages/wasp-cli.tgz")] (unlines ["allowBuilds:", "  prisma: true"])
      `shouldBe` unlines
        [ "allowBuilds:",
          "  prisma: true",
          "overrides:",
          "  \"@wasp/cli\": \"file:dev-packages/wasp-cli.tgz\""
        ]
