module AppSpec.FromJSONTest (spec_AppSpecFromJSON) where

import qualified Data.Aeson as Aeson
import Data.Aeson.Types (FromJSON)
import Data.Maybe (fromJust)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import NeatInterpolation (trimming)
import StrongPath (relfileP)
import Test.Hspec
import Wasp.AppSpec (Ref)
import qualified Wasp.AppSpec.Action as Action
import qualified Wasp.AppSpec.App.Db as Db
import qualified Wasp.AppSpec.App.EmailSender as EmailSender
import qualified Wasp.AppSpec.Core.Ref as Ref
import Wasp.AppSpec.Entity (Entity)
import qualified Wasp.AppSpec.ExtImport as ExtImport
import qualified Wasp.AppSpec.Job as Job
import Wasp.AppSpec.Page (Page)
import qualified Wasp.AppSpec.Page as Page
import Wasp.AppSpec.Query (Query)
import qualified Wasp.AppSpec.Query as Query
import qualified Wasp.AppSpec.Route as Route

spec_AppSpecFromJSON :: Spec
spec_AppSpecFromJSON = do
  describe "ExtImport" $ do
    it "parses a valid named ext import" $
      extNamedImportJson
        `shouldDecodeTo` Just
          ( ExtImport.ExtImport
              { ExtImport.name = ExtImport.ExtImportField "foo",
                ExtImport.source = ExtImport.ProjectSrcExtImportSource [relfileP|folder/file.js|],
                ExtImport.alias = Nothing
              }
          )
    it "parses a valid package ext import" $
      extPackageImportJson
        `shouldDecodeTo` Just
          ( ExtImport.ExtImport
              { ExtImport.name = ExtImport.ExtImportField "foo",
                ExtImport.source = ExtImport.PackageExtImportSource $ ExtImport.PackageImportSource "@scope/pkg" (Just "folder/file.js"),
                ExtImport.alias = Nothing
              }
          )
    it "parses a valid aliased ext import" $
      extAliasedImportJson
        `shouldDecodeTo` Just
          ( ExtImport.ExtImport
              { ExtImport.name = ExtImport.ExtImportField "foo",
                ExtImport.source = ExtImport.ProjectSrcExtImportSource [relfileP|folder/file.js|],
                ExtImport.alias = Just "bar"
              }
          )
    it "parses a valid default ext import" $
      extDefaultImportJson
        `shouldDecodeTo` Just
          ( ExtImport.ExtImport
              { ExtImport.name = ExtImport.ExtImportModule "foo",
                ExtImport.source = ExtImport.ProjectSrcExtImportSource [relfileP|folder/subfolder/file.js|],
                ExtImport.alias = Nothing
              }
          )
    it "fails to parse an invalid of import" $ do
      [trimming|
          {
            "kind": "invalid",
            "name" : "foo",
            "source": { "kind": "project-src", "path": "file.js" }
          }
        |]
        `shouldDecodeTo` (Nothing :: Maybe ExtImport.ExtImport)
  describe "Page" $ do
    it "parses a valid Page JSON with auth" $ do
      [trimming|
          {
            "component":  ${extNamedImportJson},
            "authRequired": true
          }
        |]
        `shouldDecodeTo` Just
          ( Page.Page
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
          ( Page.Page
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
        `shouldDecodeTo` (Nothing :: Maybe Page.Page)
  describe "Route" $ do
    it "parses a valid Route JSON without lazy" $ do
      [trimming|
          {
            "path": "/foo",
            "to": ${pageRef},
            "prerender": []
          }
        |]
        `shouldDecodeTo` Just
          ( Route.Route
              { Route.path = "/foo",
                Route.to = fromJust $ decodeJson pageRef,
                Route.lazy = Nothing,
                Route.prerender = []
              }
          )
    it "parses a valid Route JSON with lazy: false" $ do
      [trimming|
          {
            "path": "/foo",
            "to": ${pageRef},
            "lazy": false,
            "prerender": []
          }
        |]
        `shouldDecodeTo` Just
          ( Route.Route
              { Route.path = "/foo",
                Route.to = fromJust $ decodeJson pageRef,
                Route.lazy = Just False,
                Route.prerender = []
              }
          )
    it "parses a valid Route JSON with lazy: true" $ do
      [trimming|
          {
            "path": "/foo",
            "to": ${pageRef},
            "lazy": true,
            "prerender": []
          }
        |]
        `shouldDecodeTo` Just
          ( Route.Route
              { Route.path = "/foo",
                Route.to = fromJust $ decodeJson pageRef,
                Route.lazy = Just True,
                Route.prerender = []
              }
          )
    it "parses a valid Route JSON with a prerender path list" $ do
      [trimming|
          {
            "path": "/foo",
            "to": ${pageRef},
            "prerender": ["/foo", "/bar"]
          }
        |]
        `shouldDecodeTo` Just
          ( Route.Route
              { Route.path = "/foo",
                Route.to = fromJust $ decodeJson pageRef,
                Route.lazy = Nothing,
                Route.prerender = ["/foo", "/bar"]
              }
          )

  describe "Ref" $ do
    it "parses a valid Entity Ref JSON" $ do
      fooEntityRef `shouldDecodeTo` Just (Ref.Ref "foo" :: Ref Entity)
    it "parses a vlid Page Ref JSON" $
      pageRef `shouldDecodeTo` Just (Ref.Ref "foo" :: Ref Page)
    it "parses a valid Query Ref JSON" $ do
      [trimming|
          {
            "name": "foo",
            "declType": "Query"
          }
        |]
        `shouldDecodeTo` Just (Ref.Ref "foo" :: Ref Query)
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
          ( Query.Query
              { fn = fromJust $ decodeJson extNamedImportJson,
                entities = sequence [decodeJson fooEntityRef, decodeJson barEntityRef],
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
          ( Query.Query
              { fn = fromJust $ decodeJson extNamedImportJson,
                entities = Nothing,
                auth = Nothing
              }
          )
  describe "Action" $ do
    it "parses a valid Action JSON with auth and entities" $ do
      [trimming|
          {
            "fn": ${extNamedImportJson},
            "entities": [${fooEntityRef}, ${barEntityRef}],
            "auth": true
          }
        |]
        `shouldDecodeTo` Just
          ( Action.Action
              { fn = fromJust $ decodeJson extNamedImportJson,
                entities = sequence [decodeJson fooEntityRef, decodeJson barEntityRef],
                auth = Just True
              }
          )
    it "parses a valid Action JSON without auth and entities" $ do
      [trimming|
        {
          "fn": ${extNamedImportJson}
        }
      |]
        `shouldDecodeTo` Just
          ( Action.Action
              { fn = fromJust $ decodeJson extNamedImportJson,
                entities = Nothing,
                auth = Nothing
              }
          )
  describe "ExecutorOptions" $ do
    it "parses valid ExecutorOptions JSON for pgBoss" $ do
      [trimming|
          {
            "pgBoss": { "retryLimit": 1 }
          }
        |]
        `shouldDecodeTo` Just
          ( Job.ExecutorOptions
              { pgBoss = Aeson.decode "{ \"retryLimit\": 1 }"
              }
          )
  describe "Schedule" $ do
    it "parses a valid Schedule JSON with args and executorOptions" $ do
      [trimming|
          {
            "cron": "*/5 * * * *",
            "args": { "foo": "bar" },
            "executorOptions": {
              "pgBoss":  { "retryLimit": 1 }
            }
          }
        |]
        `shouldDecodeTo` Just
          ( Job.Schedule
              { cron = "*/5 * * * *",
                args = Aeson.decode "{ \"foo\": \"bar\" }",
                executorOptions =
                  Just
                    Job.ExecutorOptions
                      { pgBoss = Aeson.decode "{ \"retryLimit\": 1 }"
                      }
              }
          )
  describe "Job" $ do
    it "parses the simplest possible job JSON" $ do
      [trimming|
          {
            "executor": "PgBoss",
            "perform": {
              "fn": ${extNamedImportJson}
            }
          }
        |]
        `shouldDecodeTo` Just
          ( Job.Job
              { executor = Job.PgBoss,
                perform =
                  Job.Perform
                    { fn = fromJust $ decodeJson extNamedImportJson,
                      executorOptions = Nothing
                    },
                schedule = Nothing,
                entities = Nothing
              }
          )
    it "parses a more complex job JSON" $ do
      [trimming|
          {
            "executor": "PgBoss",
            "perform": {
              "fn": ${extNamedImportJson}
            },
            "schedule": {
              "cron": "*/5 * * * *",
              "args": { "foo": "bar" },
              "executorOptions": {
                "pgBoss":  { "retryLimit": 1 }
              }
            },
            "entities": [${fooEntityRef}, ${barEntityRef}]
          }
        |]
        `shouldDecodeTo` Just
          ( Job.Job
              { executor = Job.PgBoss,
                perform =
                  Job.Perform
                    { fn = fromJust $ decodeJson extNamedImportJson,
                      executorOptions = Nothing
                    },
                schedule =
                  Just
                    Job.Schedule
                      { cron = "*/5 * * * *",
                        args = Aeson.decode "{ \"foo\": \"bar\" }",
                        executorOptions =
                          Just
                            Job.ExecutorOptions
                              { pgBoss = Aeson.decode "{ \"retryLimit\": 1 }"
                              }
                      },
                entities = sequence [decodeJson fooEntityRef, decodeJson barEntityRef]
              }
          )
  describe "EmailProvider" $ do
    it "parses valid EmailProvider JSONs" $ do
      "\"SMTP\"" `shouldDecodeTo` Just EmailSender.SMTP
      "\"SendGrid\"" `shouldDecodeTo` Just EmailSender.SendGrid
      "\"Mailgun\"" `shouldDecodeTo` Just EmailSender.Mailgun
      "\"Dummy\"" `shouldDecodeTo` Just EmailSender.Dummy
    it "fails to parse an invalid EmailProvider JSON" $ do
      "IMadeThisUp" `shouldDecodeTo` (Nothing :: Maybe EmailSender.EmailProvider)

  describe "EmailSender" $ do
    it "parses a valid EmailSender JSON with defaultFrom" $ do
      [trimming|
          {
            "provider": "SMTP",
            "defaultFrom": {
              "name": "John Doe",
              "email": "something"
            }
          }
      |]
        `shouldDecodeTo` Just
          ( EmailSender.EmailSender
              { provider = EmailSender.SMTP,
                defaultFrom =
                  Just $
                    EmailSender.EmailFromField
                      { EmailSender.name = Just "John Doe",
                        EmailSender.email = "something"
                      }
              }
          )
  describe "Db" $ do
    it "parses a valid Db JSON" $ do
      [trimming|
          {
            "seeds": [${extNamedImportJson}],
            "prismaSetupFn": ${extNamedImportJson}
          }
      |]
        `shouldDecodeTo` Just
          ( Db.Db
              { Db.seeds = Just [fromJust $ decodeJson extNamedImportJson],
                Db.prismaSetupFn = decodeJson extNamedImportJson
              }
          )
  where
    extNamedImportJson = [trimming| { "kind": "named", "name" : "foo", "source": { "kind": "project-src", "path": "@src/folder/file.js" } }|]
    extPackageImportJson = [trimming| { "kind": "named", "name" : "foo", "source": { "kind": "package", "packageName": "@scope/pkg", "subpath": "folder/file.js" } }|]
    extAliasedImportJson = [trimming| { "kind": "named", "name" : "foo", "source": { "kind": "project-src", "path": "@src/folder/file.js" }, "alias": "bar" }|]
    extDefaultImportJson = [trimming| { "kind": "default", "name" : "foo", "source": { "kind": "project-src", "path": "@src/folder/subfolder/file.js" } }|]

    fooEntityRef = [trimming| { "name": "foo", "declType": "Entity" }|]
    barEntityRef = [trimming| { "name": "bar", "declType": "Entity" }|]
    pageRef = [trimming| { "name": "foo", "declType": "Page" }|]

    decodeJson :: (FromJSON a) => T.Text -> Maybe a
    decodeJson = Aeson.decodeStrict . TE.encodeUtf8

    shouldDecodeTo json expectedValue = do
      decodeJson json `shouldBe` expectedValue
