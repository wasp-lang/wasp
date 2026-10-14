module Generator.NpmInstall.PackageLockTest where

import Data.Aeson (Value, object, (.=))
import qualified Data.Aeson.Key as Key
import qualified Data.Map as Map
import Data.Text (Text)
import Test.Hspec (Spec, describe, it, shouldBe)
import Wasp.Generator.NpmInstall.PackageLock (getWaspLibTarballPaths, removeStaleWaspLibIntegrities)

spec_getWaspLibTarballPaths :: Spec
spec_getWaspLibTarballPaths =
  describe "getWaspLibTarballPaths"
    $ it "returns only the paths of Wasp lib tarballs"
    $ getWaspLibTarballPaths (packageLockWith [(waspLibResolved, Just "sha512-old"), (registryResolved, Just "sha512-react")])
      `shouldBe` [waspLibTarballPath]

spec_removeStaleWaspLibIntegrities :: Spec
spec_removeStaleWaspLibIntegrities =
  describe "removeStaleWaspLibIntegrities" $ do
    it "removes a Wasp lib integrity that doesn't match the tarball" $
      removeStaleWaspLibIntegrities currentIntegrities (packageLockWith [(waspLibResolved, Just "sha512-old")])
        `shouldBe` packageLockWith [(waspLibResolved, Nothing)]
    it "keeps a Wasp lib integrity that matches the tarball" $
      removeStaleWaspLibIntegrities currentIntegrities (packageLockWith [(waspLibResolved, Just "sha512-current")])
        `shouldBe` packageLockWith [(waspLibResolved, Just "sha512-current")]
    it "keeps the integrity of other packages" $
      removeStaleWaspLibIntegrities currentIntegrities (packageLockWith [(registryResolved, Just "sha512-react")])
        `shouldBe` packageLockWith [(registryResolved, Just "sha512-react")]
  where
    currentIntegrities = Map.fromList [(waspLibTarballPath, "sha512-current")]

waspLibTarballPath :: Text
waspLibTarballPath = ".wasp/out/libs/auth/wasp.sh-lib-auth-0.26.0.tgz"

waspLibResolved :: Text
waspLibResolved = "file:" <> waspLibTarballPath

registryResolved :: Text
registryResolved = "https://registry.npmjs.org/react/-/react-19.2.0.tgz"

packageLockWith :: [(Text, Maybe Text)] -> Value
packageLockWith packages =
  object
    [ "lockfileVersion" .= (3 :: Int),
      "packages"
        .= object
          [ Key.fromString ("node_modules/package-" ++ show index)
              .= object (("resolved" .= resolved) : ["integrity" .= integrity | Just integrity <- [maybeIntegrity]])
          | (index, (resolved, maybeIntegrity)) <- zip [1 :: Int ..] packages
          ]
    ]
