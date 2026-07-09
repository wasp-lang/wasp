{-# LANGUAGE TypeApplications #-}

module AppSpec.ToJSONTest (spec_AppSpecToJSON) where

import qualified Data.Aeson as Aeson
import Data.Aeson.Types (FromJSON, ToJSON)
import Data.Maybe (fromJust)
import NeatInterpolation (trimming)
import StrongPath (relfileP)
import Test.Hspec
import qualified Wasp.AppSpec.Api as Api
import qualified Wasp.AppSpec.Core.Decl as Decl
import Wasp.AppSpec.Core.Ref (Ref (..))
import Wasp.AppSpec.Entity (Entity)
import qualified Wasp.AppSpec.Entity as Entity
import qualified Wasp.AppSpec.ExtImport as ExtImport
import qualified Wasp.AppSpec.Job as Job
import Wasp.AppSpec.Page (Page)
import qualified Wasp.AppSpec.Page as Page
import qualified Wasp.AppSpec.Query as Query
import qualified Wasp.AppSpec.Route as Route
import qualified Wasp.Psl.Ast.Model as Psl.Model
import qualified Wasp.Psl.Ast.WithCtx as Psl.WithCtx

spec_AppSpecToJSON :: Spec
spec_AppSpecToJSON = do
  -- ToJSON instances must stay in sync with their FromJSON counterparts (the
  -- wire format described in packages/spec/src/appSpec.ts), so that
  -- `wasp inspect --json` prints exactly what the TS spec produces.
  describe "encode/decode round trips" $ do
    it "round trips ExtImport" $ do
      shouldRoundTrip namedExtImport
      shouldRoundTrip namedExtImport {ExtImport.alias = Just "bar"}
      shouldRoundTrip
        ExtImport.ExtImport
          { ExtImport.name = ExtImport.ExtImportModule "foo",
            ExtImport.path = [relfileP|folder/subfolder/file.js|],
            ExtImport.alias = Nothing
          }
    it "round trips Ref" $ do
      shouldRoundTrip (Ref "foo" :: Ref Entity)
      shouldRoundTrip (Ref "foo" :: Ref Page)
    it "round trips Page" $ do
      shouldRoundTrip Page.Page {Page.component = namedExtImport, Page.authRequired = Just True}
      shouldRoundTrip Page.Page {Page.component = namedExtImport, Page.authRequired = Nothing}
    it "round trips Route" $
      shouldRoundTrip
        Route.Route
          { Route.path = "/foo",
            Route.to = Ref "foo",
            Route.lazy = Just True,
            Route.prerender = ["/foo", "/bar"]
          }
    it "round trips Query" $
      shouldRoundTrip
        Query.Query
          { Query.fn = namedExtImport,
            Query.entities = Just [Ref "foo", Ref "bar"],
            Query.auth = Just True
          }
    it "round trips Api" $
      shouldRoundTrip
        Api.Api
          { Api.fn = namedExtImport,
            Api.middlewareConfigFn = Nothing,
            Api.entities = Just [Ref "foo"],
            Api.httpRoute = (Api.GET, "/foo/bar"),
            Api.auth = Just True
          }
    it "round trips Job" $
      shouldRoundTrip
        Job.Job
          { Job.executor = Job.PgBoss,
            Job.perform =
              Job.Perform
                { Job.fn = namedExtImport,
                  Job.executorOptions = Nothing
                },
            Job.schedule =
              Just
                Job.Schedule
                  { Job.cron = "*/5 * * * *",
                    Job.args = Aeson.decode "{ \"foo\": \"bar\" }",
                    Job.executorOptions =
                      Just Job.ExecutorOptions {Job.pgBoss = Aeson.decode "{ \"retryLimit\": 1 }"}
                  },
            Job.entities = Just [Ref "foo"]
          }

  describe "hand-written encodings" $ do
    it "encodes a Decl into the {declType, declName, declValue} envelope" $ do
      let page = Page.Page {Page.component = namedExtImport, Page.authRequired = Just True}
      Decl.makeDecl "MyPage" page
        `shouldEncodeTo` [trimming|
            {
              "declType": "Page",
              "declName": "MyPage",
              "declValue": {
                "component": { "kind": "named", "name": "foo", "path": "@src/folder/file.js", "alias": null },
                "authRequired": true
              }
            }
          |]
    it "encodes ExtImport with the @src/ path prefix the user wrote" $
      namedExtImport
        `shouldEncodeTo` [trimming|
            { "kind": "named", "name": "foo", "path": "@src/folder/file.js", "alias": null }
          |]
    it "encodes JobExecutor as a plain string" $
      Job.PgBoss `shouldEncodeTo` [trimming|"PgBoss"|]
    it "encodes Entity as fields plus its Prisma source (output-only shape)" $
      taskEntity
        `shouldEncodeTo` [trimming|
            {
              "fields": [
                { "name": "id", "type": "Int" },
                { "name": "user", "type": "User?" },
                { "name": "tags", "type": "Tag[]" }
              ],
              "pslSource": "id Int\nuser User?\ntags Tag[]\n"
            }
          |]
  where
    namedExtImport =
      ExtImport.ExtImport
        { ExtImport.name = ExtImport.ExtImportField "foo",
          ExtImport.path = [relfileP|folder/file.js|],
          ExtImport.alias = Nothing
        }

    taskEntity =
      Entity.makeEntity $
        Psl.Model.Body $
          Psl.WithCtx.empty . Psl.Model.ElementField
            <$> [ makePslField "id" Psl.Model.Int [],
                  makePslField "user" (Psl.Model.UserType "User") [Psl.Model.Optional],
                  makePslField "tags" (Psl.Model.UserType "Tag") [Psl.Model.List]
                ]

    makePslField name fieldType typeModifiers =
      Psl.Model.Field
        { Psl.Model._name = name,
          Psl.Model._type = fieldType,
          Psl.Model._typeModifiers = typeModifiers,
          Psl.Model._attrs = []
        }

    shouldRoundTrip :: (Eq a, Show a, ToJSON a, FromJSON a) => a -> Expectation
    shouldRoundTrip value = Aeson.decode (Aeson.encode value) `shouldBe` Just value

    shouldEncodeTo value expectedJson =
      Aeson.toJSON value `shouldBe` fromJust (Aeson.decodeStrictText expectedJson)
