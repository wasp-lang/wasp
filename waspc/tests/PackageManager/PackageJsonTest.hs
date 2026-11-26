module PackageManager.PackageJsonTest where

import Data.Aeson (Value (..), object, toJSON, (.=))
import Test.Hspec
import Wasp.PackageManager.PackageJson (hasWorkspacesField, parseDevEnginesField, parsePackageManagerField)
import Wasp.PackageManager.PackageManager (PackageManager (..))

{- HLINT ignore spec_PackageManager_PackageJson "Avoid npm and npx" -}

spec_PackageManager_PackageJson :: Spec
spec_PackageManager_PackageJson = do
  describe "parsePackageManagerField" $ do
    it "reads the name before the version" $ do
      packageManagerField "pnpm@9.12.1+sha512.abc" `shouldBe` Just Pnpm
      packageManagerField "npm@11.0.0" `shouldBe` Just Npm
      packageManagerField "bun@1.3.2" `shouldBe` Just Bun

    it "reads a name without a version" $ do
      packageManagerField "pnpm" `shouldBe` Just Pnpm
      packageManagerField "pnpm@" `shouldBe` Just Pnpm

    it "ignores anything after the name" $
      packageManagerField "pnpm@not a version " `shouldBe` Just Pnpm

    it "returns Nothing for package managers Wasp doesn't support" $ do
      packageManagerField "yarn@4.5.0" `shouldBe` Nothing
      packageManagerField "deno@2.0.0" `shouldBe` Nothing
      packageManagerField "cnpm@9.0.0" `shouldBe` Nothing

    it "returns Nothing for names that aren't exact" $ do
      packageManagerField " pnpm@9.0.0" `shouldBe` Nothing
      packageManagerField "pnpm @9.0.0" `shouldBe` Nothing
      packageManagerField "PNPM@9.0.0" `shouldBe` Nothing
      packageManagerField "^pnpm@9.0.0" `shouldBe` Nothing

    it "returns Nothing for empty or garbage values" $ do
      packageManagerField "" `shouldBe` Nothing
      packageManagerField "@9.0.0" `shouldBe` Nothing
      packageManagerField "@@@" `shouldBe` Nothing

    it "returns Nothing when the field isn't a string" $ do
      parsePackageManagerField (packageJson ["packageManager" .= (9 :: Int)]) `shouldBe` Nothing
      parsePackageManagerField (packageJson ["packageManager" .= Null]) `shouldBe` Nothing
      parsePackageManagerField (packageJson ["packageManager" .= object ["name" .= ("pnpm" :: String)]]) `shouldBe` Nothing

    it "returns Nothing without the field" $
      parsePackageManagerField (packageJson []) `shouldBe` Nothing

    it "returns Nothing when package.json isn't an object" $
      parsePackageManagerField (String "pnpm@9.0.0") `shouldBe` Nothing

  describe "parseDevEnginesField" $ do
    it "reads a single package manager" $
      devEnginesField (object ["name" .= ("bun" :: String)]) `shouldBe` Just Bun

    it "ignores the other fields of a package manager" $
      devEnginesField (object ["name" .= ("pnpm" :: String), "version" .= ("^10" :: String), "onFail" .= ("warn" :: String)])
        `shouldBe` Just Pnpm

    it "reads the first of several alternatives" $
      devEnginesField (toJSONList [object ["name" .= ("pnpm" :: String)], object ["name" .= ("npm" :: String)]])
        `shouldBe` Just Pnpm

    it "skips alternatives Wasp doesn't support or can't read" $
      devEnginesField
        ( toJSONList
            [ object ["name" .= ("yarn" :: String)],
              String "garbage",
              object ["version" .= ("1" :: String)],
              object ["name" .= ("bun" :: String)]
            ]
        )
        `shouldBe` Just Bun

    it "returns Nothing for package managers Wasp doesn't support" $
      devEnginesField (object ["name" .= ("deno" :: String)]) `shouldBe` Nothing

    it "returns Nothing for names that aren't exact" $ do
      devEnginesField (object ["name" .= (" pnpm" :: String)]) `shouldBe` Nothing
      devEnginesField (object ["name" .= ("pnpm@10" :: String)]) `shouldBe` Nothing
      devEnginesField (object ["name" .= ("Pnpm" :: String)]) `shouldBe` Nothing

    it "returns Nothing for garbage values" $ do
      devEnginesField (object ["name" .= ("" :: String)]) `shouldBe` Nothing
      devEnginesField (object ["name" .= (10 :: Int)]) `shouldBe` Nothing
      devEnginesField (object []) `shouldBe` Nothing
      devEnginesField (String "pnpm") `shouldBe` Nothing
      devEnginesField (toJSONList []) `shouldBe` Nothing
      parseDevEnginesField (packageJson ["devEngines" .= ("pnpm" :: String)]) `shouldBe` Nothing

    it "returns Nothing without the field" $ do
      parseDevEnginesField (packageJson []) `shouldBe` Nothing
      parseDevEnginesField (packageJson ["devEngines" .= object []]) `shouldBe` Nothing

  describe "hasWorkspacesField" $ do
    it "accepts the array and the object form" $ do
      hasWorkspacesField (packageJson ["workspaces" .= ["apps/*" :: String]]) `shouldBe` True
      hasWorkspacesField (packageJson ["workspaces" .= object ["packages" .= ["apps/*" :: String]]]) `shouldBe` True

    it "accepts empty workspaces" $ do
      hasWorkspacesField (packageJson ["workspaces" .= ([] :: [String])]) `shouldBe` True
      hasWorkspacesField (packageJson ["workspaces" .= object []]) `shouldBe` True

    it "rejects a package.json without workspaces" $ do
      hasWorkspacesField (packageJson []) `shouldBe` False
      hasWorkspacesField (String "workspaces") `shouldBe` False

    it "rejects garbage workspaces values" $ do
      hasWorkspacesField (packageJson ["workspaces" .= Null]) `shouldBe` False
      hasWorkspacesField (packageJson ["workspaces" .= ("apps/*" :: String)]) `shouldBe` False
      hasWorkspacesField (packageJson ["workspaces" .= True]) `shouldBe` False
  where
    packageJson = object . (("name" .= ("app" :: String)) :)
    packageManagerField value = parsePackageManagerField $ packageJson ["packageManager" .= (value :: String)]
    devEnginesField packageManager = parseDevEnginesField $ packageJson ["devEngines" .= object ["packageManager" .= packageManager]]
    toJSONList = toJSON :: [Value] -> Value
