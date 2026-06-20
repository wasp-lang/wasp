module Wasp.Cli.Command.SpecDump
  ( specDump,
  )
where

import Control.Monad.IO.Class (liftIO)
import Data.Aeson (Value (..), object, (.=), toJSON)
import Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified StrongPath as SP
import qualified Wasp.AppSpec as AS
import qualified Wasp.AppSpec.App as AS.App
import qualified Wasp.AppSpec.App.Auth as AS.App.Auth
import qualified Wasp.AppSpec.App.Auth.EmailVerification as AS.App.Auth.EmailVerification
import qualified Wasp.AppSpec.App.Auth.PasswordReset as AS.App.Auth.PasswordReset
import qualified Wasp.AppSpec.App.Client as AS.App.Client
import qualified Wasp.AppSpec.App.Db as AS.App.Db
import qualified Wasp.AppSpec.App.EmailSender as AS.App.EmailSender
import qualified Wasp.AppSpec.App.Server as AS.App.Server
import qualified Wasp.AppSpec.App.Wasp as AS.App.Wasp
import qualified Wasp.AppSpec.App.WebSocket as AS.App.WebSocket
import qualified Wasp.AppSpec.Api as AS.Api
import qualified Wasp.AppSpec.ApiNamespace as AS.ApiNamespace
import qualified Wasp.AppSpec.Action as AS.Action
import qualified Wasp.AppSpec.Core.Ref as Ref
import qualified Wasp.AppSpec.Crud as AS.Crud
import qualified Wasp.AppSpec.Entity as AS.Entity
import qualified Wasp.AppSpec.Entity.Field as AS.Entity.Field
import qualified Wasp.AppSpec.ExtImport as AS.ExtImport
import qualified Wasp.AppSpec.JSON as AS.JSON
import qualified Wasp.AppSpec.Job as AS.Job
import qualified Wasp.AppSpec.Page as AS.Page
import qualified Wasp.AppSpec.Query as AS.Query
import qualified Wasp.AppSpec.Route as AS.Route
import qualified Wasp.AppSpec.Valid as ASV
import Wasp.Cli.Command (Command)
import Wasp.Cli.Command.Compile (analyze)
import Wasp.Cli.Command.Require (InWaspProject (InWaspProject), WaspSpecAvailable (WaspSpecAvailable), require)

specDump :: Command ()
specDump = do
  InWaspProject waspProjectDir <- require
  WaspSpecAvailable <- require
  appSpec <- analyze waspProjectDir
  liftIO $ BL.putStrLn $ encodePretty $ specToJson appSpec

specToJson :: AS.AppSpec -> Value
specToJson spec = object
  [ "app" .= appToJson (snd $ ASV.getApp spec)
  , "pages" .= map (uncurry pageToJson) (AS.getPages spec)
  , "routes" .= map (uncurry routeToJson) (AS.getRoutes spec)
  , "queries" .= map (uncurry queryToJson) (AS.getQueries spec)
  , "actions" .= map (uncurry actionToJson) (AS.getActions spec)
  , "apis" .= map (uncurry apiToJson) (AS.getApis spec)
  , "apiNamespaces" .= map (uncurry apiNamespaceToJson) (AS.getApiNamespaces spec)
  , "jobs" .= map (uncurry jobToJson) (AS.getJobs spec)
  , "cruds" .= map (uncurry crudToJson) (AS.getCruds spec)
  , "entities" .= map (uncurry entityToJson) (AS.getEntities spec)
  , "prismaSchema" .= show (AS.prismaSchema spec)
  , "buildType" .= show (AS.buildType spec)
  ]

extImportToJson :: AS.ExtImport.ExtImport -> Value
extImportToJson imp = object
  [ "name" .= case AS.ExtImport.name imp of
      AS.ExtImport.ExtImportModule n -> n
      AS.ExtImport.ExtImportField n -> n
  , "kind" .= case AS.ExtImport.name imp of
      AS.ExtImport.ExtImportModule _ -> ("default" :: String)
      AS.ExtImport.ExtImportField _ -> ("named" :: String)
  , "path" .= SP.toFilePath (AS.ExtImport.path imp)
  , "alias" .= AS.ExtImport.alias imp
  ]

maybeExtImport :: Maybe AS.ExtImport.ExtImport -> Value
maybeExtImport = maybe Null extImportToJson

refToJson :: Ref.Ref a -> Value
refToJson ref = object
  [ "name" .= Ref.refName ref
  ]

maybeRefList :: Maybe [Ref.Ref a] -> Value
maybeRefList = maybe Null (toJSON . map refToJson)

appToJson :: AS.App.App -> Value
appToJson app = object
  [ "wasp" .= object ["version" .= AS.App.Wasp.version (AS.App.wasp app)]
  , "title" .= AS.App.title app
  , "head" .= maybe Null toJSON (AS.App.head app)
  , "auth" .= maybeAuth (AS.App.auth app)
  , "server" .= maybeServer (AS.App.server app)
  , "client" .= maybeClient (AS.App.client app)
  , "db" .= maybeDb (AS.App.db app)
  , "emailSender" .= maybeEmailSender (AS.App.emailSender app)
  , "webSocket" .= maybeWebSocket (AS.App.webSocket app)
  ]

maybeAuth :: Maybe AS.App.Auth.Auth -> Value
maybeAuth = maybe Null authToJson

authToJson :: AS.App.Auth.Auth -> Value
authToJson auth = object
  [ "userEntity" .= refToJson (AS.App.Auth.userEntity auth)
  , "methods" .= authMethodsToJson (AS.App.Auth.methods auth)
  , "onAuthFailedRedirectTo" .= AS.App.Auth.onAuthFailedRedirectTo auth
  , "onAuthSucceededRedirectTo" .= AS.App.Auth.onAuthSucceededRedirectTo auth
  , "onBeforeSignup" .= maybeExtImport (AS.App.Auth.onBeforeSignup auth)
  , "onAfterSignup" .= maybeExtImport (AS.App.Auth.onAfterSignup auth)
  , "onAfterEmailVerified" .= maybeExtImport (AS.App.Auth.onAfterEmailVerified auth)
  , "onBeforeOAuthRedirect" .= maybeExtImport (AS.App.Auth.onBeforeOAuthRedirect auth)
  , "onBeforeLogin" .= maybeExtImport (AS.App.Auth.onBeforeLogin auth)
  , "onAfterLogin" .= maybeExtImport (AS.App.Auth.onAfterLogin auth)
  ]

authMethodsToJson :: AS.App.Auth.AuthMethods -> Value
authMethodsToJson methods = object
  [ "usernameAndPassword" .= maybeSignupConfig (AS.App.Auth.usernameAndPassword methods)
  , "slack" .= maybeExternalAuthConfig (AS.App.Auth.slack methods)
  , "discord" .= maybeExternalAuthConfig (AS.App.Auth.discord methods)
  , "google" .= maybeExternalAuthConfig (AS.App.Auth.google methods)
  , "gitHub" .= maybeExternalAuthConfig (AS.App.Auth.gitHub methods)
  , "keycloak" .= maybeExternalAuthConfig (AS.App.Auth.keycloak methods)
  , "microsoft" .= maybeExternalAuthConfig (AS.App.Auth.microsoft methods)
  , "email" .= maybeEmailAuthConfig (AS.App.Auth.email methods)
  ]

maybeSignupConfig :: Maybe AS.App.Auth.UsernameAndPasswordConfig -> Value
maybeSignupConfig = maybe Null signupConfigToJson

signupConfigToJson :: AS.App.Auth.UsernameAndPasswordConfig -> Value
signupConfigToJson c = object
  [ "userSignupFields" .= maybeExtImport (AS.App.Auth.userSignupFields c)
  ]

maybeExternalAuthConfig :: Maybe AS.App.Auth.ExternalAuthConfig -> Value
maybeExternalAuthConfig = maybe Null externalAuthConfigToJson

externalAuthConfigToJson :: AS.App.Auth.ExternalAuthConfig -> Value
externalAuthConfigToJson c = object
  [ "configFn" .= maybeExtImport (AS.App.Auth.configFn c)
  , "userSignupFields" .= maybeExtImport (AS.App.Auth.userSignupFields c)
  ]

maybeEmailAuthConfig :: Maybe AS.App.Auth.EmailAuthConfig -> Value
maybeEmailAuthConfig = maybe Null emailAuthConfigToJson

emailAuthConfigToJson :: AS.App.Auth.EmailAuthConfig -> Value
emailAuthConfigToJson c = object
  [ "userSignupFields" .= maybeExtImport (AS.App.Auth.userSignupFields c)
  , "fromField" .= emailFromFieldToJson (AS.App.Auth.fromField c)
  , "emailVerification" .= emailVerificationToJson (AS.App.Auth.emailVerification c)
  , "passwordReset" .= passwordResetToJson (AS.App.Auth.passwordReset c)
  ]

emailFromFieldToJson :: AS.App.EmailSender.EmailFromField -> Value
emailFromFieldToJson f = object
  [ "name" .= AS.App.EmailSender.name f
  , "email" .= AS.App.EmailSender.email f
  ]

emailVerificationToJson :: AS.App.Auth.EmailVerification.EmailVerificationConfig -> Value
emailVerificationToJson c = object
  [ "getEmailContentFn" .= maybeExtImport (AS.App.Auth.EmailVerification.getEmailContentFn c)
  , "clientRoute" .= refToJson (AS.App.Auth.EmailVerification.clientRoute c)
  ]

passwordResetToJson :: AS.App.Auth.PasswordReset.PasswordResetConfig -> Value
passwordResetToJson c = object
  [ "getEmailContentFn" .= maybeExtImport (AS.App.Auth.PasswordReset.getEmailContentFn c)
  , "clientRoute" .= refToJson (AS.App.Auth.PasswordReset.clientRoute c)
  ]

maybeServer :: Maybe AS.App.Server.Server -> Value
maybeServer = maybe Null serverToJson

serverToJson :: AS.App.Server.Server -> Value
serverToJson s = object
  [ "setupFn" .= maybeExtImport (AS.App.Server.setupFn s)
  , "middlewareConfigFn" .= maybeExtImport (AS.App.Server.middlewareConfigFn s)
  , "envValidationSchema" .= maybeExtImport (AS.App.Server.envValidationSchema s)
  ]

maybeClient :: Maybe AS.App.Client.Client -> Value
maybeClient = maybe Null clientToJson

clientToJson :: AS.App.Client.Client -> Value
clientToJson c = object
  [ "setupFn" .= maybeExtImport (AS.App.Client.setupFn c)
  , "rootComponent" .= maybeExtImport (AS.App.Client.rootComponent c)
  , "baseDir" .= AS.App.Client.baseDir c
  , "envValidationSchema" .= maybeExtImport (AS.App.Client.envValidationSchema c)
  ]

maybeDb :: Maybe AS.App.Db.Db -> Value
maybeDb = maybe Null dbToJson

dbToJson :: AS.App.Db.Db -> Value
dbToJson db = object
  [ "seeds" .= maybe Null (toJSON . map extImportToJson) (AS.App.Db.seeds db)
  , "prismaSetupFn" .= maybeExtImport (AS.App.Db.prismaSetupFn db)
  ]

maybeEmailSender :: Maybe AS.App.EmailSender.EmailSender -> Value
maybeEmailSender = maybe Null emailSenderToJson

emailSenderToJson :: AS.App.EmailSender.EmailSender -> Value
emailSenderToJson es = object
  [ "provider" .= show (AS.App.EmailSender.provider es)
  , "defaultFrom" .= maybeEmailFromField (AS.App.EmailSender.defaultFrom es)
  ]

maybeEmailFromField :: Maybe AS.App.EmailSender.EmailFromField -> Value
maybeEmailFromField = maybe Null emailFromFieldToJson

maybeWebSocket :: Maybe AS.App.WebSocket.WebSocket -> Value
maybeWebSocket = maybe Null webSocketToJson

webSocketToJson :: AS.App.WebSocket.WebSocket -> Value
webSocketToJson ws = object
  [ "fn" .= extImportToJson (AS.App.WebSocket.fn ws)
  , "autoConnect" .= AS.App.WebSocket.autoConnect ws
  ]

pageToJson :: String -> AS.Page.Page -> Value
pageToJson name page = object
  [ "name" .= name
  , "component" .= extImportToJson (AS.Page.component page)
  , "authRequired" .= AS.Page.authRequired page
  ]

routeToJson :: String -> AS.Route.Route -> Value
routeToJson name route = object
  [ "name" .= name
  , "path" .= AS.Route.path route
  , "to" .= refToJson (AS.Route.to route)
  , "lazy" .= AS.Route.lazy route
  , "prerender" .= AS.Route.prerender route
  ]

queryToJson :: String -> AS.Query.Query -> Value
queryToJson name query = object
  [ "name" .= name
  , "fn" .= extImportToJson (AS.Query.fn query)
  , "entities" .= maybeRefList (AS.Query.entities query)
  , "auth" .= AS.Query.auth query
  ]

actionToJson :: String -> AS.Action.Action -> Value
actionToJson name action = object
  [ "name" .= name
  , "fn" .= extImportToJson (AS.Action.fn action)
  , "entities" .= maybeRefList (AS.Action.entities action)
  , "auth" .= AS.Action.auth action
  ]

apiToJson :: String -> AS.Api.Api -> Value
apiToJson name api = object
  [ "name" .= name
  , "fn" .= extImportToJson (AS.Api.fn api)
  , "middlewareConfigFn" .= maybeExtImport (AS.Api.middlewareConfigFn api)
  , "entities" .= maybeRefList (AS.Api.entities api)
  , "httpRoute" .= object
    [ "method" .= show (fst (AS.Api.httpRoute api))
    , "path" .= snd (AS.Api.httpRoute api)
    ]
  , "auth" .= AS.Api.auth api
  ]

apiNamespaceToJson :: String -> AS.ApiNamespace.ApiNamespace -> Value
apiNamespaceToJson name ns = object
  [ "name" .= name
  , "middlewareConfigFn" .= extImportToJson (AS.ApiNamespace.middlewareConfigFn ns)
  , "path" .= AS.ApiNamespace.path ns
  ]

jobToJson :: String -> AS.Job.Job -> Value
jobToJson name job = object
  [ "name" .= name
  , "executor" .= show (AS.Job.executor job)
  , "perform" .= performToJson (AS.Job.perform job)
  , "schedule" .= maybeSchedule (AS.Job.schedule job)
  , "entities" .= maybeRefList (AS.Job.entities job)
  ]

performToJson :: AS.Job.Perform -> Value
performToJson p = object
  [ "fn" .= extImportToJson (AS.Job.fn p)
  , "executorOptions" .= maybeExecutorOptions (AS.Job.executorOptions p)
  ]

maybeSchedule :: Maybe AS.Job.Schedule -> Value
maybeSchedule = maybe Null scheduleToJson

scheduleToJson :: AS.Job.Schedule -> Value
scheduleToJson s = object
  [ "cron" .= AS.Job.cron s
  , "args" .= maybe Null toJSON (AS.Job.args s)
  , "executorOptions" .= maybeExecutorOptions (AS.Job.executorOptions s)
  ]

maybeExecutorOptions :: Maybe AS.Job.ExecutorOptions -> Value
maybeExecutorOptions = maybe Null executorOptionsToJson

executorOptionsToJson :: AS.Job.ExecutorOptions -> Value
executorOptionsToJson opts = object
  [ "pgBoss" .= maybe Null toJSON (AS.Job.pgBoss opts)
  ]

crudToJson :: String -> AS.Crud.Crud -> Value
crudToJson name crud = object
  [ "name" .= name
  , "entity" .= refToJson (AS.Crud.entity crud)
  , "operations" .= crudOperationsToJson (AS.Crud.operations crud)
  ]

crudOperationsToJson :: AS.Crud.CrudOperations -> Value
crudOperationsToJson ops = object
  [ "get" .= maybeOperationOptions (AS.Crud.get ops)
  , "getAll" .= maybeOperationOptions (AS.Crud.getAll ops)
  , "create" .= maybeOperationOptions (AS.Crud.create ops)
  , "update" .= maybeOperationOptions (AS.Crud.update ops)
  , "delete" .= maybeOperationOptions (AS.Crud.delete ops)
  ]

maybeOperationOptions :: Maybe AS.Crud.CrudOperationOptions -> Value
maybeOperationOptions = maybe Null operationOptionsToJson

operationOptionsToJson :: AS.Crud.CrudOperationOptions -> Value
operationOptionsToJson opts = object
  [ "isPublic" .= AS.Crud.isPublic opts
  , "overrideFn" .= maybeExtImport (AS.Crud.overrideFn opts)
  ]

entityToJson :: String -> AS.Entity.Entity -> Value
entityToJson name entity = object
  [ "name" .= name
  , "fields" .= toJSON (map fieldToJson (AS.Entity.getFields entity))
  ]

fieldToJson :: AS.Entity.Field.Field -> Value
fieldToJson f = object
  [ "name" .= AS.Entity.Field.fieldName f
  , "type" .= show (AS.Entity.Field.fieldType f)
  ]
