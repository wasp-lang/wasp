module AppSpec.InspectableTest (spec_AppSpecInspectable) where

import StrongPath (relfileP)
import Test.Hspec
import qualified Wasp.AppSpec.Api as Api
import qualified Wasp.AppSpec.App as App
import qualified Wasp.AppSpec.App.Auth as Auth
import qualified Wasp.AppSpec.App.Wasp as Wasp
import Wasp.AppSpec.Core.Inspectable (Inspectable (..), InspectionEntry (..))
import Wasp.AppSpec.Core.Ref (Ref (..))
import qualified Wasp.AppSpec.Crud as Crud
import qualified Wasp.AppSpec.ExtImport as ExtImport
import qualified Wasp.AppSpec.Job as Job
import qualified Wasp.AppSpec.Page as Page
import qualified Wasp.AppSpec.Query as Query
import qualified Wasp.AppSpec.Route as Route

spec_AppSpecInspectable :: Spec
spec_AppSpecInspectable = do
  describe "showExtImport" $ do
    it "renders a named import" $
      ExtImport.showExtImport (namedExtImport "getTasks")
        `shouldBe` "{ getTasks } from \"@src/queries.ts\""
    it "renders a default import" $
      ExtImport.showExtImport
        ExtImport.ExtImport
          { ExtImport.name = ExtImport.ExtImportModule "Main",
            ExtImport.path = [relfileP|pages/Main.tsx|],
            ExtImport.alias = Nothing
          }
        `shouldBe` "Main from \"@src/pages/Main.tsx\""
    it "renders an aliased import" $
      ExtImport.showExtImport
        (namedExtImport "getTasks") {ExtImport.alias = Just "fetchTasks"}
        `shouldBe` "{ getTasks as fetchTasks } from \"@src/queries.ts\""
    it "does not render an alias equal to the imported name" $
      ExtImport.showExtImport
        (namedExtImport "getTasks") {ExtImport.alias = Just "getTasks"}
        `shouldBe` "{ getTasks } from \"@src/queries.ts\""

  describe "inspect" $ do
    it "describes a Route" $
      inspect
        ( "HomeRoute",
          Route.Route
            { Route.path = "/",
              Route.to = Ref "HomePage",
              Route.lazy = Just True,
              Route.prerender = ["/"]
            }
        )
        `shouldBe` InspectionEntry ["/", "HomeRoute", "-> HomePage", "[lazy] [prerender]"]
    it "describes a Page" $
      inspect
        ( "HomePage",
          Page.Page
            { Page.component = namedExtImport "HomePage",
              Page.authRequired = Just True
            }
        )
        `shouldBe` InspectionEntry ["HomePage", "[auth]", "{ HomePage } from \"@src/queries.ts\""]
    it "describes a Query" $
      inspect
        ( "getTasks",
          Query.Query
            { Query.fn = namedExtImport "getTasks",
              Query.entities = Just [Ref "Task", Ref "Tag"],
              Query.auth = Nothing
            }
        )
        `shouldBe` InspectionEntry ["getTasks", "", "{ getTasks } from \"@src/queries.ts\"", "uses Task, Tag"]
    it "describes an Api" $
      inspect
        ( "fooBar",
          Api.Api
            { Api.fn = namedExtImport "fooBar",
              Api.middlewareConfigFn = Nothing,
              Api.entities = Nothing,
              Api.httpRoute = (Api.POST, "/foo/bar"),
              Api.auth = Just True
            }
        )
        `shouldBe` InspectionEntry ["POST /foo/bar", "fooBar", "[auth]", "{ fooBar } from \"@src/queries.ts\"", ""]
    it "describes a Crud with operation options" $
      inspect
        ( "tasks",
          Crud.Crud
            { Crud.entity = Ref "Task",
              Crud.operations =
                Crud.CrudOperations
                  { Crud.get = Just $ Crud.CrudOperationOptions {Crud.isPublic = Just True, Crud.overrideFn = Nothing},
                    Crud.getAll = Just $ Crud.CrudOperationOptions {Crud.isPublic = Nothing, Crud.overrideFn = Just $ namedExtImport "getAllOverride"},
                    Crud.create = Just $ Crud.CrudOperationOptions {Crud.isPublic = Nothing, Crud.overrideFn = Nothing},
                    Crud.update = Nothing,
                    Crud.delete = Nothing
                  }
            }
        )
        `shouldBe` InspectionEntry ["tasks", "on Task", "get (public), getAll (override), create"]
    it "describes a Job" $
      inspect
        ( "mySpecialJob",
          Job.Job
            { Job.executor = Job.PgBoss,
              Job.perform = Job.Perform {Job.fn = namedExtImport "mySpecialJob", Job.executorOptions = Nothing},
              Job.schedule = Just Job.Schedule {Job.cron = "0 * * * *", Job.args = Nothing, Job.executorOptions = Nothing},
              Job.entities = Just [Ref "Task"]
            }
        )
        `shouldBe` InspectionEntry
          ["mySpecialJob", "PgBoss", "cron \"0 * * * *\"", "{ mySpecialJob } from \"@src/queries.ts\"", "uses Task"]
    it "describes an App with auth" $
      inspect ("TodoApp", app)
        `shouldBe` InspectionEntry
          ["TodoApp", "\"ToDo App\"", "auth: google (user entity: User)"]
    it "describes an App without auth" $
      inspect ("TodoApp", app {App.auth = Nothing})
        `shouldBe` InspectionEntry ["TodoApp", "\"ToDo App\"", ""]
  where
    namedExtImport name =
      ExtImport.ExtImport
        { ExtImport.name = ExtImport.ExtImportField name,
          ExtImport.path = [relfileP|queries.ts|],
          ExtImport.alias = Nothing
        }

    app =
      App.App
        { App.wasp = Wasp.Wasp {Wasp.version = "^0.25.0"},
          App.title = "ToDo App",
          App.head = Nothing,
          App.auth = Just auth,
          App.server = Nothing,
          App.client = Nothing,
          App.db = Nothing,
          App.emailSender = Nothing,
          App.webSocket = Nothing
        }

    auth =
      Auth.Auth
        { Auth.userEntity = Ref "User",
          Auth.methods =
            Auth.AuthMethods
              { Auth.usernameAndPassword = Nothing,
                Auth.slack = Nothing,
                Auth.discord = Nothing,
                Auth.google = Just Auth.ExternalAuthConfig {Auth.configFn = Nothing, Auth.userSignupFields = Nothing},
                Auth.gitHub = Nothing,
                Auth.keycloak = Nothing,
                Auth.microsoft = Nothing,
                Auth.email = Nothing
              },
          Auth.onAuthFailedRedirectTo = "/login",
          Auth.onAuthSucceededRedirectTo = Nothing,
          Auth.onBeforeSignup = Nothing,
          Auth.onAfterSignup = Nothing,
          Auth.onAfterEmailVerified = Nothing,
          Auth.onBeforeOAuthRedirect = Nothing,
          Auth.onBeforeLogin = Nothing,
          Auth.onAfterLogin = Nothing
        }
