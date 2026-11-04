module SemanticVersion.VersionTest where

import Data.Either (isLeft)
import Test.Hspec
import qualified Text.Parsec as P
import Wasp.SemanticVersion.Version

spec_SemanticVersion_Version :: Spec
spec_SemanticVersion_Version = do
  describe "show" $ do
    it "shows release versions" $ do
      show (releaseVersion 1 2 3) `shouldBe` "1.2.3"
      show (releaseVersion 0 2 3) `shouldBe` "0.2.3"
      show (releaseVersion 0 0 3) `shouldBe` "0.0.3"
      show (releaseVersion 0 0 0) `shouldBe` "0.0.0"

    it "shows pre-release versions" $ do
      show (Version 1 2 3 [AlphanumericIdentifier "alpha"] []) `shouldBe` "1.2.3-alpha"
      show (Version 1 2 3 [AlphanumericIdentifier "alpha", NumericIdentifier 1] []) `shouldBe` "1.2.3-alpha.1"
      show (Version 1 2 3 [NumericIdentifier 0] []) `shouldBe` "1.2.3-0"
      show (Version 1 2 3 [NumericIdentifier 1, NumericIdentifier 2, NumericIdentifier 3] []) `shouldBe` "1.2.3-1.2.3"

    it "shows versions with build" $ do
      show (Version 1 2 3 [] ["build"]) `shouldBe` "1.2.3+build"
      show (Version 1 2 3 [] ["build", "001"]) `shouldBe` "1.2.3+build.001"

    it "shows pre-release versions with build" $ do
      show (Version 1 2 3 [AlphanumericIdentifier "rc", NumericIdentifier 1] ["sha", "5114f85"]) `shouldBe` "1.2.3-rc.1+sha.5114f85"

  describe "parseVersion" $ do
    it "parses release versions" $ do
      parseVersion "1.2.3" `shouldBe` Right (releaseVersion 1 2 3)
      parseVersion "103.20.35" `shouldBe` Right (releaseVersion 103 20 35)
      parseVersion "0.1.33" `shouldBe` Right (releaseVersion 0 1 33)
      parseVersion "0.0.0" `shouldBe` Right (releaseVersion 0 0 0)
      parseVersion "1.0.1" `shouldBe` Right (releaseVersion 1 0 1)

    -- Cases from `node-semver`'s test/fixtures/valid-versions.js.
    it "parses pre-release versions" $ do
      (.preRelease) <$> parseVersion "1.2.3-0" `shouldBe` Right [NumericIdentifier 0]
      (.preRelease) <$> parseVersion "1.2.3-123" `shouldBe` Right [NumericIdentifier 123]
      (.preRelease) <$> parseVersion "1.2.3-1.2.3" `shouldBe` Right [NumericIdentifier 1, NumericIdentifier 2, NumericIdentifier 3]
      (.preRelease) <$> parseVersion "1.2.3-1a" `shouldBe` Right [AlphanumericIdentifier "1a"]
      (.preRelease) <$> parseVersion "1.2.3-a1" `shouldBe` Right [AlphanumericIdentifier "a1"]
      (.preRelease) <$> parseVersion "1.2.3-alpha" `shouldBe` Right [AlphanumericIdentifier "alpha"]
      (.preRelease) <$> parseVersion "1.2.3-alpha.1" `shouldBe` Right [AlphanumericIdentifier "alpha", NumericIdentifier 1]
      (.preRelease) <$> parseVersion "1.2.3-alpha-1" `shouldBe` Right [AlphanumericIdentifier "alpha-1"]
      (.preRelease) <$> parseVersion "1.2.3-alpha-.-beta" `shouldBe` Right [AlphanumericIdentifier "alpha-", AlphanumericIdentifier "-beta"]
      (.preRelease) <$> parseVersion "1.2.3-01a" `shouldBe` Right [AlphanumericIdentifier "01a"]
      (.preRelease) <$> parseVersion "1.0.0---" `shouldBe` Right [AlphanumericIdentifier "--"]

    it "parses versions with build" $ do
      (.buildMetadata) <$> parseVersion "1.2.3+456" `shouldBe` Right ["456"]
      (.buildMetadata) <$> parseVersion "1.2.3+build" `shouldBe` Right ["build"]
      (.buildMetadata) <$> parseVersion "1.2.3+new-build" `shouldBe` Right ["new-build"]
      (.buildMetadata) <$> parseVersion "1.2.3+build.1" `shouldBe` Right ["build", "1"]
      (.buildMetadata) <$> parseVersion "1.2.3+build.1a" `shouldBe` Right ["build", "1a"]
      (.buildMetadata) <$> parseVersion "1.2.3+build.a1" `shouldBe` Right ["build", "a1"]
      (.buildMetadata) <$> parseVersion "1.2.3+build.alpha.beta" `shouldBe` Right ["build", "alpha", "beta"]
      (.buildMetadata) <$> parseVersion "1.2.3+001" `shouldBe` Right ["001"]
      (.buildMetadata) <$> parseVersion "1.2.3+b-c.d" `shouldBe` Right ["b-c", "d"]

    it "parses pre-release versions with build" $ do
      let parsedVersion = parseVersion "1.2.3-alpha.1+build.5"
      parsedVersion `shouldBe` Right (Version 1 2 3 [AlphanumericIdentifier "alpha", NumericIdentifier 1] [])
      (.buildMetadata) <$> parsedVersion `shouldBe` Right ["build", "5"]

    it "round trips through show" $ do
      let roundTrip versionString = show <$> parseVersion versionString `shouldBe` Right versionString
      roundTrip "1.2.3"
      roundTrip "1.2.3-0"
      roundTrip "1.2.3-alpha.1"
      roundTrip "1.2.3-alpha-.-beta"
      roundTrip "1.2.3+build.001"
      roundTrip "1.2.3-rc.1+sha.5114f85"

    it "rejects partial versions" $ do
      isLeft (parseVersion "1") `shouldBe` True
      isLeft (parseVersion "1.2") `shouldBe` True
      isLeft (parseVersion "1.2-alpha") `shouldBe` True
      isLeft (parseVersion "1-alpha") `shouldBe` True

    it "rejects invalid formats" $ do
      isLeft (parseVersion "01.2.3") `shouldBe` True
      isLeft (parseVersion "1.02.3") `shouldBe` True
      isLeft (parseVersion "1.2.03") `shouldBe` True
      isLeft (parseVersion "v1.2.3") `shouldBe` True
      isLeft (parseVersion "1.2.x") `shouldBe` True
      isLeft (parseVersion "1.x.x") `shouldBe` True
      isLeft (parseVersion "*") `shouldBe` True
      isLeft (parseVersion ".2.3") `shouldBe` True
      isLeft (parseVersion "foo") `shouldBe` True
      isLeft (parseVersion "") `shouldBe` True

    it "rejects invalid pre-release formats" $ do
      isLeft (parseVersion "1.2.3-") `shouldBe` True
      isLeft (parseVersion "1.2.3-01") `shouldBe` True
      isLeft (parseVersion "1.2.3-alpha.01") `shouldBe` True
      isLeft (parseVersion "1.2.3-alpha.") `shouldBe` True
      isLeft (parseVersion "1.2.3-.alpha") `shouldBe` True
      isLeft (parseVersion "1.2.3-alpha..1") `shouldBe` True
      isLeft (parseVersion "1.2.3-alpha_1") `shouldBe` True
      isLeft (parseVersion "1.2.3-é") `shouldBe` True
      isLeft (parseVersion "1.2.3alpha") `shouldBe` True

    it "rejects invalid build formats" $ do
      isLeft (parseVersion "1.2.3+") `shouldBe` True
      isLeft (parseVersion "1.2.3+build.") `shouldBe` True
      isLeft (parseVersion "1.2.3+.build") `shouldBe` True
      isLeft (parseVersion "1.2.3+build..1") `shouldBe` True
      isLeft (parseVersion "1.2.3+build_1") `shouldBe` True
      isLeft (parseVersion "1.2.3+é") `shouldBe` True
      isLeft (parseVersion "1.2.3+a+b") `shouldBe` True

  describe "versionParser" $ do
    let looseParseVersion = P.parse versionParser ""

    it "parses full versions with trailing content" $ do
      looseParseVersion "1.2.3.4.5.6.7.8.9.0" `shouldBe` Right (releaseVersion 1 2 3)
      looseParseVersion "1.2.3foobar" `shouldBe` Right (releaseVersion 1 2 3)
      looseParseVersion "1.2.3 some other stuff" `shouldBe` Right (releaseVersion 1 2 3)

    it "parses the valid pre-release and build prefix" $ do
      show <$> looseParseVersion "1.2.3-alpha.1 some other stuff" `shouldBe` Right "1.2.3-alpha.1"
      show <$> looseParseVersion "1.2.3-alpha.01" `shouldBe` Right "1.2.3-alpha"
      show <$> looseParseVersion "1.2.3-alpha." `shouldBe` Right "1.2.3-alpha"
      show <$> looseParseVersion "1.2.3-alpha+build.5_6" `shouldBe` Right "1.2.3-alpha+build.5"
      show <$> looseParseVersion "1.2.3+build.5+6" `shouldBe` Right "1.2.3+build.5"

    it "ignores invalid pre-release and build parts" $ do
      show <$> looseParseVersion "1.2.3-" `shouldBe` Right "1.2.3"
      show <$> looseParseVersion "1.2.3-01" `shouldBe` Right "1.2.3"
      show <$> looseParseVersion "1.2.3-_" `shouldBe` Right "1.2.3"
      show <$> looseParseVersion "1.2.3+" `shouldBe` Right "1.2.3"
      show <$> looseParseVersion "1.2.3-alpha+" `shouldBe` Right "1.2.3-alpha"

  describe "Eq" $ do
    it "ignores build" $ do
      [v|1.2.3+build|] `shouldBe` [v|1.2.3|]
      [v|1.2.3+build.1|] `shouldBe` [v|1.2.3+build.2|]
      [v|1.2.3-alpha+build|] `shouldBe` [v|1.2.3-alpha|]

    it "doesn't ignore pre-release" $ do
      [v|1.2.3-alpha|] `shouldNotBe` [v|1.2.3|]
      [v|1.2.3-alpha|] `shouldNotBe` [v|1.2.3-beta|]
      [v|1.2.3-alpha|] `shouldNotBe` [v|1.2.3-alpha.0|]
      [v|1.2.3-1|] `shouldNotBe` [v|1.2.3-01a|]

  describe "Ord" $ do
    let greaterVersion ≻ smallerVersion =
          it (show greaterVersion <> " > " <> show smallerVersion) $ do
            compare greaterVersion smallerVersion `shouldBe` GT
            compare smallerVersion greaterVersion `shouldBe` LT

    -- Release versions.
    [v|0.0.1|] ≻ [v|0.0.0|]
    [v|1.0.0|] ≻ [v|0.9.9|]
    [v|0.10.0|] ≻ [v|0.9.0|]
    [v|0.99.0|] ≻ [v|0.10.0|]
    [v|2.0.0|] ≻ [v|1.2.3|]
    [v|1.10.0|] ≻ [v|1.9.0|]
    [v|1.0.10|] ≻ [v|1.0.9|]

    -- Release versions have higher precedence than their pre-release versions.
    [v|0.0.0|] ≻ [v|0.0.0-foo|]
    [v|1.2.3|] ≻ [v|1.2.3-asdf|]
    [v|1.2.3|] ≻ [v|1.2.3-4|]
    [v|1.2.3|] ≻ [v|1.2.3-4-foo|]
    [v|1.2.3|] ≻ [v|1.2.3-0|]

    -- Major, minor and patch have higher priority than pre-release.
    [v|1.2.4-0|] ≻ [v|1.2.3|]
    [v|1.3.0-alpha|] ≻ [v|1.2.9|]
    [v|2.0.0-0|] ≻ [v|1.99.99|]

    -- Example from the SemVer specification: https://semver.org/#spec-item-11
    [v|1.0.0-alpha.1|] ≻ [v|1.0.0-alpha|]
    [v|1.0.0-alpha.beta|] ≻ [v|1.0.0-alpha.1|]
    [v|1.0.0-beta|] ≻ [v|1.0.0-alpha.beta|]
    [v|1.0.0-beta.2|] ≻ [v|1.0.0-beta|]
    [v|1.0.0-beta.11|] ≻ [v|1.0.0-beta.2|]
    [v|1.0.0-rc.1|] ≻ [v|1.0.0-beta.11|]
    [v|1.0.0|] ≻ [v|1.0.0-rc.1|]

    -- Cases from `node-semver`'s test/fixtures/comparisons.js.
    [v|1.2.3-5-foo|] ≻ [v|1.2.3-5|]
    [v|1.2.3-5|] ≻ [v|1.2.3-4|]
    [v|1.2.3-5-foo|] ≻ [v|1.2.3-5-Foo|]
    [v|3.0.0|] ≻ [v|2.7.2+asdf|]
    [v|1.2.3-a.10|] ≻ [v|1.2.3-a.5|]
    [v|1.2.3-a.b|] ≻ [v|1.2.3-a.5|]
    [v|1.2.3-a.b|] ≻ [v|1.2.3-a|]
    [v|1.2.3-a.b.c.10.d.5|] ≻ [v|1.2.3-a.b.c.5.d.100|]
    [v|1.2.3-r2|] ≻ [v|1.2.3-r100|]
    [v|1.2.3-r100|] ≻ [v|1.2.3-R2|]

    -- Numeric identifiers are compared numerically and have lower precedence than alphanumeric ones.
    [v|1.2.3-10|] ≻ [v|1.2.3-9|]
    [v|1.2.3-a|] ≻ [v|1.2.3-999|]
    [v|1.2.3-0a|] ≻ [v|1.2.3-999|]
    [v|1.2.3--|] ≻ [v|1.2.3-999|]

    -- Build is ignored.
    it "1.2.3+b == 1.2.3+a" $ compare [v|1.2.3+b|] [v|1.2.3+a|] `shouldBe` EQ
    it "1.2.3-alpha+b == 1.2.3-alpha" $ compare [v|1.2.3-alpha+b|] [v|1.2.3-alpha|] `shouldBe` EQ

  it "releaseVersion" $ do
    releaseVersion 1 2 3 `shouldBe` Version 1 2 3 [] []
    show (releaseVersion 1 2 3) `shouldBe` "1.2.3"

  it "lowestVersion" $ do
    lowestVersion `shouldBe` [v|0.0.0-0|]
    lowestVersion < [v|0.0.0-alpha|] `shouldBe` True
    lowestVersion < [v|0.0.0|] `shouldBe` True

  it "lowestPreReleaseOf" $ do
    lowestPreReleaseOf 2 0 0 `shouldBe` [v|2.0.0-0|]
    lowestPreReleaseOf 2 0 0 < [v|2.0.0-alpha|] `shouldBe` True
    lowestPreReleaseOf 2 0 0 > [v|1.99.99|] `shouldBe` True

  it "isPreRelease" $ do
    isPreRelease [v|1.2.3|] `shouldBe` False
    isPreRelease [v|1.2.3+build|] `shouldBe` False
    isPreRelease [v|1.2.3-0|] `shouldBe` True
    isPreRelease [v|1.2.3-alpha+build|] `shouldBe` True

  it "hasSameCoreAs" $ do
    [v|1.2.3|] `hasSameCoreAs` [v|1.2.3|] `shouldBe` True
    [v|1.2.3-alpha|] `hasSameCoreAs` [v|1.2.3|] `shouldBe` True
    [v|1.2.3-alpha|] `hasSameCoreAs` [v|1.2.3-beta+build|] `shouldBe` True
    [v|1.2.3|] `hasSameCoreAs` [v|1.2.4|] `shouldBe` False
    [v|1.2.3-alpha|] `hasSameCoreAs` [v|1.3.3-alpha|] `shouldBe` False
    [v|1.2.3-alpha|] `hasSameCoreAs` [v|2.2.3-alpha|] `shouldBe` False

  it "nextBreakingChangeVersion" $ do
    nextBreakingChangeVersion [v|1.2.3|] `shouldBe` [v|2.0.0|]
    nextBreakingChangeVersion [v|0.2.3|] `shouldBe` [v|0.3.0|]
    nextBreakingChangeVersion [v|0.0.3|] `shouldBe` [v|0.0.4|]
    nextBreakingChangeVersion [v|1.2.3-alpha+build|] `shouldBe` [v|2.0.0|]
    nextBreakingChangeVersion [v|0.2.3-alpha|] `shouldBe` [v|0.3.0|]
    nextBreakingChangeVersion [v|0.0.3-alpha|] `shouldBe` [v|0.0.4|]

  it "v quasi quoter" $ do
    [v|1.2.3|] `shouldBe` releaseVersion 1 2 3
    [v|0.0.0|] `shouldBe` releaseVersion 0 0 0
    [v|103.20.35|] `shouldBe` releaseVersion 103 20 35
    [v|1.2.3-alpha.1|] `shouldBe` Version 1 2 3 [AlphanumericIdentifier "alpha", NumericIdentifier 1] []
    [v|1.2.3-alpha.1+build|].buildMetadata `shouldBe` ["build"]
