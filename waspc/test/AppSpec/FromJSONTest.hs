module AppSpec.FromJSONTest where

import qualified Data.Aeson as Aeson
import Data.Aeson.Types (FromJSON)
import Data.Maybe (fromJust)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import NeatInterpolation (trimming)
import StrongPath (relfileP)
import Test.Tasty.Hspec
import Wasp.AppSpec
import Wasp.AppSpec.Core.Ref
import Wasp.AppSpec.Entity
import Wasp.AppSpec.ExtImport
import Wasp.AppSpec.Page
import Wasp.AppSpec.Query

spec_AppSpecFromJSON :: Spec
spec_AppSpecFromJSON = do
  describe "ExtImport" $ do
    it "parses a valid named ext import" $
      extNamedImportJson
        `shouldDecodeTo` Just
          ( ExtImport
              { name = ExtImportField "foo",
                path = [relfileP|file.js|]
              }
          )
    it "parses a valid default ext import" $
      extDefaultImportJson
        `shouldDecodeTo` Just
          ( ExtImport
              { name = ExtImportModule "foo",
                path = [relfileP|file.js|]
              }
          )
    it "fails to parse an invalid of import" $ do
      [trimming|
          {
            "kind": "invalid",
            "name" : "foo",
            "path": "file.js" 
          }
        |]
        `shouldDecodeTo` (Nothing :: Maybe ExtImport)
  describe "Page" $ do
    it "parses a valid Page JSON with auth" $ do
      [trimming|
          {
            "component":  ${extNamedImportJson},
            "authRequired": true
          }
        |]
        `shouldDecodeTo` Just
          ( Page
              { component = fromJust $ decodeJson extNamedImportJson,
                authRequired = Just True
              }
          )
    it "parses a valid Page JSON without auth" $ do
      [trimming|
          {
            "component":  ${extNamedImportJson}
          }
        |]
        `shouldDecodeTo` Just
          ( Page
              { component = fromJust $ decodeJson extNamedImportJson,
                authRequired = Nothing
              }
          )
    it "fails to parse a Page JSON without a component" $ do
      [trimming|
          {
            "authRequired": true
          }
        |]
        `shouldDecodeTo` (Nothing :: Maybe Page)
  describe "Ref" $ do
    it "parses a valid Entity Ref JSON" $ do
      fooEntityRef `shouldDecodeTo` Just (Ref "foo" :: Ref Entity)
    it "parses a vlid Page Ref JSON" $
      pageRef `shouldDecodeTo` Just (Ref "foo" :: Ref Page)
    it "parses a valid Query Ref JSON" $ do
      [trimming|
          {
            "name": "foo",
            "declType": "Query"
          }
        |]
        `shouldDecodeTo` Just (Ref "foo" :: Ref Query)
    it "fails to parse an invalid declType" $ do
      [trimming|
          {
            "name": "foo",
            "declType": "IMadeThisUp"
          }
        |]
        -- How to properly type this?
        `shouldDecodeTo` (Nothing :: Maybe (Ref Entity))
  describe "Query" $ do
    it "parses a valid Query JSON with auth and entities" $ do
      [trimming|
          {
            "fn": ${extNamedImportJson},
            "entities": [${fooEntityRef}, ${barEntityRef}],
            "auth": true
          }
        |]
        `shouldDecodeTo` Just
          ( Query
              { fn = fromJust $ decodeJson extNamedImportJson,
                entities = Just [Ref "foo", Ref "bar"],
                auth = Just True
              }
          )
    it "parses a valid Query JSON without auth and entities" $ do
      [trimming|
          {
            "fn": ${extNamedImportJson}
          }
        |]
        `shouldDecodeTo` Just
          ( Query
              { fn = fromJust $ decodeJson extNamedImportJson,
                entities = Nothing,
                auth = Nothing
              }
          )
  where
    extNamedImportJson = [trimming| { "kind": "named", "name" : "foo", "path": "file.js" }|]
    extDefaultImportJson = [trimming| { "kind": "default", "name" : "foo", "path": "file.js" }|]

    fooEntityRef = [trimming| { "name": "foo", "declType": "Entity" }|]
    barEntityRef = [trimming| { "name": "bar", "declType": "Entity" }|]
    pageRef = [trimming| { "name": "foo", "declType": "Page" }|]

    decodeJson :: FromJSON a => T.Text -> Maybe a
    decodeJson = Aeson.decodeStrict . TE.encodeUtf8

    shouldDecodeTo json expectedValue = do
      decodeJson json `shouldBe` expectedValue
