module PackageManager.UserAgentTest where

import Test.Hspec
import Wasp.PackageManager.PackageManager (PackageManager (..))
import Wasp.PackageManager.UserAgent (parseUserAgent)

{- HLINT ignore spec_PackageManager_UserAgent "Avoid npm and npx" -}

spec_PackageManager_UserAgent :: Spec
spec_PackageManager_UserAgent = do
  describe "parseUserAgent" $ do
    it "reads the package manager from the first product" $ do
      parseUserAgent "npm/10.9.0 node/v22.11.0 darwin arm64 workspaces/false" `shouldBe` Just Npm
      parseUserAgent "pnpm/9.13.2 npm/? node/v22.11.0 darwin arm64" `shouldBe` Just Pnpm
      parseUserAgent "bun/1.1.38 npm/? node/v22.6.0 darwin arm64" `shouldBe` Just Bun

    it "reads a product without a version" $ do
      parseUserAgent "pnpm" `shouldBe` Just Pnpm
      parseUserAgent "pnpm/ node/v22.11.0" `shouldBe` Just Pnpm

    it "ignores surrounding whitespace" $ do
      parseUserAgent "  pnpm/9.13.2 npm/? node/v22.11.0" `shouldBe` Just Pnpm
      parseUserAgent "\tbun/1.1.38\n" `shouldBe` Just Bun

    it "only reads the first product" $ do
      parseUserAgent "node/v22.11.0 pnpm/9.13.2" `shouldBe` Nothing
      parseUserAgent "npminstall/7.0.0 npm/? node/v22.11.0" `shouldBe` Nothing

    it "returns Nothing for package managers Wasp doesn't support" $ do
      parseUserAgent "yarn/1.22.22 npm/? node/v22.11.0 darwin arm64" `shouldBe` Nothing
      parseUserAgent "deno/2.0.0 npm/? deno/2.0.0 darwin arm64" `shouldBe` Nothing
      parseUserAgent "cnpm/9.0.0 npm/? node/v22.11.0" `shouldBe` Nothing

    it "returns Nothing for names that aren't exact" $ do
      parseUserAgent "PNPM/9.13.2" `shouldBe` Nothing
      parseUserAgent "pnpm9.13.2" `shouldBe` Nothing
      parseUserAgent "pnpm@9.13.2" `shouldBe` Nothing

    it "returns Nothing for empty or garbage values" $ do
      parseUserAgent "" `shouldBe` Nothing
      parseUserAgent "   " `shouldBe` Nothing
      parseUserAgent "/9.13.2 pnpm/9.13.2" `shouldBe` Nothing
      parseUserAgent "!@#$%^&*" `shouldBe` Nothing
