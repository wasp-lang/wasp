module Generator.ImportAliasesTest where

import Data.Aeson ((.:))
import qualified Data.Aeson as Aeson
import Data.List (stripPrefix)
import qualified Data.Map as M
import Data.Maybe (mapMaybe)
import Test.Hspec
import Wasp.Generator.ImportAliases (makeSdkImportAliases, readSdkExports)

spec_makeSdkImportAliases :: Spec
spec_makeSdkImportAliases = do
  it "maps an SDK export to the file it points at, without the JS extension" $ do
    makeSdkImportAliases
      ( M.fromList
          [ ("./client/operations", "./dist/client/operations/index.js"),
            ("./client/app", "./dist/client/app/index.jsx"),
            ("./client/api", "./dist/api/index.js")
          ]
      )
      `shouldBe` M.fromList
        [ ("wasp/client/operations", ["./sdk/wasp/dist/client/operations/index"]),
          ("wasp/client/app", ["./sdk/wasp/dist/client/app/index"]),
          ("wasp/client/api", ["./sdk/wasp/dist/api/index"])
        ]

  it "keeps the wildcard of an export" $ do
    makeSdkImportAliases (M.fromList [("./server/crud/*", "./dist/server/crud/*.js")])
      `shouldBe` M.fromList [("wasp/server/crud/*", ["./sdk/wasp/dist/server/crud/*"])]

-- | The project's tsconfig extends the tsconfig of \@wasp/wasp, which has to
-- map every import that the generated SDK exports.
spec_waspNpmPackageTsConfig :: Spec
spec_waspNpmPackageTsConfig = do
  describe "packages/wasp/tsconfig.paths.json" $ do
    it "maps the same SDK imports as the tsconfig that Wasp generates for the server" $ do
      generatedSdkAliases <- makeSdkImportAliases <$> readSdkExports
      packageAliases <- readWaspNpmPackageTsConfigPaths
      let packageSdkAliases = M.mapMaybe (mapM $ stripPrefix sdkPathPrefixInPackageTsConfig) packageAliases
      packageSdkAliases `shouldBe` M.map (mapMaybe $ stripPrefix sdkPathPrefixInGeneratedTsConfig) generatedSdkAliases
  where
    -- The tsconfig of the package finds the generated app dir from the dir of
    -- the tsconfig that extends it, which is the project's.
    sdkPathPrefixInPackageTsConfig = "${configDir}/.wasp/out/"
    -- The generated tsconfig is in the generated app dir itself.
    sdkPathPrefixInGeneratedTsConfig = "./"

    readWaspNpmPackageTsConfigPaths :: IO (M.Map String [FilePath])
    readWaspNpmPackageTsConfigPaths =
      either (error . ("Couldn't parse " ++) . (waspNpmPackageTsConfigPath ++) . (": " ++)) (return . tsConfigPaths)
        =<< Aeson.eitherDecodeFileStrict' waspNpmPackageTsConfigPath

    -- Tests run with waspc/ as the working directory.
    waspNpmPackageTsConfigPath = "../packages/wasp/tsconfig.paths.json"

newtype WaspNpmPackageTsConfig = WaspNpmPackageTsConfig {tsConfigPaths :: M.Map String [FilePath]}

instance Aeson.FromJSON WaspNpmPackageTsConfig where
  parseJSON = Aeson.withObject "WaspNpmPackageTsConfig" $ \tsConfig -> do
    compilerOptions <- tsConfig .: "compilerOptions"
    WaspNpmPackageTsConfig <$> compilerOptions .: "paths"
