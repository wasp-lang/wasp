{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Generator.FileDraft.BytesFileDraftTest where

import qualified Data.Aeson as Aeson
import Data.Text.Lazy.Encoding (encodeUtf8)
import Fixtures (systemSPRoot)
import GHC.Generics
import qualified Generator.MockWriteableMonad as Mock
import qualified StrongPath as SP
import Test.Tasty.Hspec (Spec, describe, it, shouldBe)
import Wasp.Generator.FileDraft
  ( Writeable (write),
    createBytesFileDraftFromJson,
  )

data Person = Person
  { name :: String,
    score :: Int
  }
  deriving (Show, Eq, Generic, Aeson.ToJSON, Aeson.FromJSON)

spec_JsonFileDraft :: Spec
spec_JsonFileDraft = do
  describe "write" $ do
    it "Creates new BytesFileDraft from JSON" $ do
      let mock = write dstDir fileDraft
      let mockLogs = Mock.getMockLogs mock Mock.defaultMockConfig
      Mock.createDirectoryIfMissing_calls mockLogs
        `shouldBe` [(True, SP.toFilePath $ SP.parent expectedDstPath)]
      Mock.writeFileFromByteString_calls mockLogs
        `shouldBe` [(SP.toFilePath expectedDstPath, encodeUtf8 "{\"score\":107,\"name\":\"Foo Barson\"}")]
  where
    (dstDir, dstPath) =
      ( systemSPRoot SP.</> [SP.reldir|a/b|],
        [SP.relfile|c/d/dst.json|]
      )
    person = Person {name = "Foo Barson", score = 107}
    fileDraft = createBytesFileDraftFromJson dstPath (Aeson.toJSON person)
    expectedDstPath = dstDir SP.</> dstPath
