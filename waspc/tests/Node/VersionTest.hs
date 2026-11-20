module Node.VersionTest where

import Test.Hspec
import Wasp.Node.Version (isRangeInWaspSupportedRange, oldestWaspSupportedNodeVersion)
import qualified Wasp.SemanticVersion as SV

spec_NodeVersion :: Spec
spec_NodeVersion =
  describe "isRangeInWaspSupportedRange" $ do
    let oldestVersion = oldestWaspSupportedNodeVersion
        nextMajorVersion = SV.nextBreakingChangeVersion oldestVersion
        range ~> expected =
          it (show range) $ isRangeInWaspSupportedRange range `shouldBe` expected

    SV.backwardsCompatibleWith oldestVersion ~> True
    SV.approximatelyEquivalentTo oldestVersion ~> True
    SV.eq oldestVersion ~> True
    SV.hyphenRange oldestVersion nextMajorVersion ~> False
    -- Same as `node-semver`'s `subset`: `<25.0.0` is rejected because its bounds include 25.0.0 pre-releases.
    (SV.gte oldestVersion `intersect` SV.lt nextMajorVersion {SV.preRelease = [SV.NumericIdentifier 0]}) ~> True
    (SV.gte oldestVersion `intersect` SV.lt nextMajorVersion) ~> False
    (SV.gte oldestVersion `intersect` SV.lte nextMajorVersion) ~> False
    (SV.gte oldestVersion `intersect` SV.lt nextMajorVersion {SV.preRelease = [SV.AlphanumericIdentifier "rc"]}) ~> False
    SV.gte oldestVersion ~> False
    SV.backwardsCompatibleWith oldestVersion {SV.minor = 0, SV.patch = 0} ~> False
    SV.backwardsCompatibleWith oldestVersion {SV.preRelease = [SV.AlphanumericIdentifier "rc"]} ~> False
    SV.backwardsCompatibleWith oldestVersion {SV.patch = oldestVersion.patch + 1} ~> True
  where
    intersect :: SV.Range -> SV.Range -> SV.Range
    intersect (SV.Range rangeExpressions1) (SV.Range rangeExpressions2) =
      SV.Range $ (<>) <$> rangeExpressions1 <*> rangeExpressions2
