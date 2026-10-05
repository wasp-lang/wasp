module NodePackageFFITest where

import Data.Aeson (Value, object, (.=))
import qualified Data.Aeson as Aeson
import Test.Hspec
import Wasp.NodePackageFFI (removeDevDependencies)

spec_removeDevDependencies :: Spec
spec_removeDevDependencies = do
  it "removes devDependencies and keeps everything else" $ do
    removeDevDependencies packageJsonWithDevDependencies `shouldBe` packageJsonWithoutDevDependencies

  it "leaves a package.json without devDependencies unchanged" $ do
    removeDevDependencies packageJsonWithoutDevDependencies `shouldBe` packageJsonWithoutDevDependencies

  it "leaves non-object JSON unchanged" $ do
    removeDevDependencies (Aeson.String "not a package.json") `shouldBe` Aeson.String "not a package.json"
  where
    packageJsonWithDevDependencies :: Value
    packageJsonWithDevDependencies =
      object
        [ "name" .= ("@wasp.sh/spec" :: String),
          "version" .= ("0.26.0" :: String),
          "dependencies" .= object ["typescript" .= ("^6.0.3" :: String)],
          "devDependencies" .= object ["eslint" .= ("^9.9.0" :: String)]
        ]

    packageJsonWithoutDevDependencies :: Value
    packageJsonWithoutDevDependencies =
      object
        [ "name" .= ("@wasp.sh/spec" :: String),
          "version" .= ("0.26.0" :: String),
          "dependencies" .= object ["typescript" .= ("^6.0.3" :: String)]
        ]
