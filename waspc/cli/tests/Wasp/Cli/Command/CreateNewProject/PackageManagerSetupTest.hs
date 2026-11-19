module Wasp.Cli.Command.CreateNewProject.PackageManagerSetupTest where

import qualified Data.Text as T
import Test.Hspec
import Wasp.Cli.Command.CreateNewProject.PackageManagerSetup
  ( makePnpmWorkspaceFileContent,
    removeAllowScriptsField,
    useCommandsOfPackageManager,
  )
import Wasp.Project.PackageManager (PackageManager (..))

spec_removeAllowScriptsField :: Spec
spec_removeAllowScriptsField = do
  it "removes the field when it is the last one, with the comma before it" $ do
    removeAllowScriptsField
      ( T.unlines
          [ "{",
            "  \"name\": \"app\",",
            "  \"devDependencies\": {",
            "    \"vite\": \"^8.1.0\"",
            "  },",
            "  \"allowScripts\": {",
            "    \"prisma\": true,",
            "    \"esbuild\": true",
            "  }",
            "}"
          ]
      )
      `shouldBe` T.unlines
        [ "{",
          "  \"name\": \"app\",",
          "  \"devDependencies\": {",
          "    \"vite\": \"^8.1.0\"",
          "  }",
          "}"
        ]

  it "removes the field when another one follows" $ do
    removeAllowScriptsField
      ( T.unlines
          [ "{",
            "  \"allowScripts\": {",
            "    \"prisma\": true",
            "  },",
            "  \"name\": \"app\"",
            "}"
          ]
      )
      `shouldBe` T.unlines ["{", "  \"name\": \"app\"", "}"]

  it "changes nothing when it can't find the field in the expected shape" $ do
    let withoutField = T.unlines ["{", "  \"name\": \"app\"", "}"]
    let onOneLine = "{ \"allowScripts\": { \"prisma\": true }, \"name\": \"app\" }"
    removeAllowScriptsField withoutField `shouldBe` withoutField
    removeAllowScriptsField onOneLine `shouldBe` onOneLine

spec_makePnpmWorkspaceFileContent :: Spec
spec_makePnpmWorkspaceFileContent = do
  it "allows the scripts of the given packages" $ do
    lines (makePnpmWorkspaceFileContent ["prisma", "@prisma/engines"])
      `shouldContain` ["allowBuilds:", "  \"prisma\": true", "  \"@prisma/engines\": true"]

spec_useCommandsOfPackageManager :: Spec
spec_useCommandsOfPackageManager = do
  it "turns npm's commands into the package manager's" $ do
    useCommandsOfPackageManager Pnpm "Run `npm install`, then `npm run wasp -- db migrate-dev` and `npm run dev`."
      `shouldBe` "Run `pnpm install`, then `pnpm run wasp db migrate-dev` and `pnpm run dev`."
    useCommandsOfPackageManager Bun "Run `npm run wasp -- compile`."
      `shouldBe` "Run `bun run wasp compile`."

  it "leaves the commands that create a project alone" $ do
    useCommandsOfPackageManager Pnpm "npx @wasp/cli@latest new <project-name>"
      `shouldBe` "npx @wasp/cli@latest new <project-name>"
