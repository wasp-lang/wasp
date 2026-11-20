// The app's spec, as `wasp show spec --json` prints it. Only generated in
// development, since it describes the app's internals. Typed as `unknown` so
// TypeScript doesn't infer a type from the whole literal.
export const spec: unknown = {
  "dbSystem": "PostgreSQL",
  "decls": [
    {
      "declType": "Entity",
      "declValue": {
        "fields": [
          {
            "name": "id",
            "type": "Int"
          },
          {
            "name": "isOnAfterSignupHookCalled",
            "type": "Boolean"
          },
          {
            "name": "isOnAfterLoginHookCalled",
            "type": "Boolean"
          },
          {
            "name": "isOnAfterEmailVerifiedHookCalled",
            "type": "Boolean"
          },
          {
            "name": "numTimesOnAfterEmailVerifiedCalled",
            "type": "Int"
          },
          {
            "name": "tasks",
            "type": "Task[]"
          },
          {
            "name": "address",
            "type": "String?"
          },
          {
            "name": "votes",
            "type": "TaskVote[]"
          },
          {
            "name": "uppercaseTextRequests",
            "type": "UppercaseTextRequest[]"
          }
        ],
        "name": "User"
      }
    },
    {
      "declType": "Entity",
      "declValue": {
        "fields": [
          {
            "name": "id",
            "type": "Int"
          },
          {
            "name": "description",
            "type": "String"
          },
          {
            "name": "isDone",
            "type": "Boolean"
          },
          {
            "name": "user",
            "type": "User"
          },
          {
            "name": "userId",
            "type": "Int"
          },
          {
            "name": "votes",
            "type": "TaskVote[]"
          },
          {
            "name": "visibility",
            "type": "TaskVisibility"
          }
        ],
        "name": "Task"
      }
    },
    {
      "declType": "Entity",
      "declValue": {
        "fields": [
          {
            "name": "id",
            "type": "String"
          },
          {
            "name": "user",
            "type": "User"
          },
          {
            "name": "userId",
            "type": "Int"
          },
          {
            "name": "task",
            "type": "Task"
          },
          {
            "name": "taskId",
            "type": "Int"
          }
        ],
        "name": "TaskVote"
      }
    },
    {
      "declType": "Entity",
      "declValue": {
        "fields": [
          {
            "name": "id",
            "type": "String"
          },
          {
            "name": "input",
            "type": "String"
          },
          {
            "name": "output",
            "type": "String?"
          },
          {
            "name": "user",
            "type": "User"
          },
          {
            "name": "userId",
            "type": "Int"
          },
          {
            "name": "state",
            "type": "UppercaseTextRequestState"
          },
          {
            "name": "createdAt",
            "type": "DateTime"
          }
        ],
        "name": "UppercaseTextRequest"
      }
    },
    {
      "declType": "App",
      "declValue": {
        "auth": {
          "methods": {
            "discord": {
              "configFn": {
                "alias": "discordConfig",
                "kind": "named",
                "name": "discordConfig",
                "path": "./src/features/auth/providers/discord"
              },
              "userSignupFields": {
                "alias": "discordUserSignupFields",
                "kind": "named",
                "name": "discordUserSignupFields",
                "path": "./src/features/auth/providers/discord"
              }
            },
            "email": {
              "emailVerification": {
                "clientRoute": {
                  "declType": "Route",
                  "name": "EmailVerificationRoute"
                },
                "getEmailContentFn": {
                  "alias": "getVerificationEmailContent",
                  "kind": "named",
                  "name": "getVerificationEmailContent",
                  "path": "./src/features/auth/providers/email"
                }
              },
              "fromField": {
                "email": "kitchen-sink@wasp.sh",
                "name": "Wasp's \"Kitchen Sink\""
              },
              "passwordReset": {
                "clientRoute": {
                  "declType": "Route",
                  "name": "PasswordResetRoute"
                },
                "getEmailContentFn": {
                  "alias": "getPasswordResetEmailContent",
                  "kind": "named",
                  "name": "getPasswordResetEmailContent",
                  "path": "./src/features/auth/providers/email"
                }
              },
              "passwordValidationSchema": {
                "alias": "passwordValidationSchema",
                "kind": "named",
                "name": "passwordValidationSchema",
                "path": "./src/features/auth/password"
              },
              "userSignupFields": {
                "alias": "emailUserSignupFields",
                "kind": "named",
                "name": "emailUserSignupFields",
                "path": "./src/features/auth/providers/email"
              }
            },
            "gitHub": {
              "configFn": {
                "alias": "gitHubConfig",
                "kind": "named",
                "name": "gitHubConfig",
                "path": "./src/features/auth/providers/github"
              },
              "userSignupFields": {
                "alias": "gitHubUserSignupFields",
                "kind": "named",
                "name": "gitHubUserSignupFields",
                "path": "./src/features/auth/providers/github"
              }
            },
            "google": {
              "configFn": {
                "alias": "googleConfig",
                "kind": "named",
                "name": "googleConfig",
                "path": "./src/features/auth/providers/google"
              },
              "userSignupFields": {
                "alias": "googleUserSignupFields",
                "kind": "named",
                "name": "googleUserSignupFields",
                "path": "./src/features/auth/providers/google"
              }
            },
            "keycloak": null,
            "microsoft": {
              "configFn": {
                "alias": "microsoftConfig",
                "kind": "named",
                "name": "microsoftConfig",
                "path": "./src/features/auth/providers/microsoft"
              },
              "userSignupFields": {
                "alias": "microsoftUserSignupFields",
                "kind": "named",
                "name": "microsoftUserSignupFields",
                "path": "./src/features/auth/providers/microsoft"
              }
            },
            "slack": {
              "configFn": {
                "alias": "slackConfig",
                "kind": "named",
                "name": "slackConfig",
                "path": "./src/features/auth/providers/slack"
              },
              "userSignupFields": {
                "alias": "slackUserSignupFields",
                "kind": "named",
                "name": "slackUserSignupFields",
                "path": "./src/features/auth/providers/slack"
              }
            },
            "usernameAndPassword": null
          },
          "onAfterEmailVerified": {
            "alias": "onAfterEmailVerified",
            "kind": "named",
            "name": "onAfterEmailVerified",
            "path": "./src/features/auth/hooks"
          },
          "onAfterLogin": {
            "alias": "onAfterLogin",
            "kind": "named",
            "name": "onAfterLogin",
            "path": "./src/features/auth/hooks"
          },
          "onAfterSignup": {
            "alias": "onAfterSignup",
            "kind": "named",
            "name": "onAfterSignup",
            "path": "./src/features/auth/hooks"
          },
          "onAuthFailedRedirectTo": "/login",
          "onAuthSucceededRedirectTo": "/",
          "onBeforeLogin": {
            "alias": "onBeforeLogin",
            "kind": "named",
            "name": "onBeforeLogin",
            "path": "./src/features/auth/hooks"
          },
          "onBeforeOAuthRedirect": null,
          "onBeforeSignup": {
            "alias": "onBeforeSignup",
            "kind": "named",
            "name": "onBeforeSignup",
            "path": "./src/features/auth/hooks"
          },
          "userEntity": {
            "declType": "Entity",
            "name": "User"
          }
        },
        "client": {
          "baseDir": null,
          "envValidationSchema": {
            "alias": "clientEnvValidationSchema",
            "kind": "named",
            "name": "clientEnvValidationSchema",
            "path": "./src/env"
          },
          "onError": {
            "alias": "onClientError",
            "kind": "named",
            "name": "onClientError",
            "path": "./src/features/errors/clientOnError"
          },
          "rootComponent": {
            "alias": "App",
            "kind": "named",
            "name": "App",
            "path": "./src/App"
          },
          "setupFn": {
            "alias": "clientSetup",
            "kind": "named",
            "name": "clientSetup",
            "path": "./src/clientSetup"
          }
        },
        "db": {
          "prismaSetupFn": {
            "alias": "setUpPrisma",
            "kind": "named",
            "name": "setUpPrisma",
            "path": "./src/features/db/prisma"
          },
          "seeds": [
            {
              "alias": "devSeedSimple",
              "kind": "named",
              "name": "devSeedSimple",
              "path": "./src/features/db/seeds"
            },
            {
              "alias": "prodSeed",
              "kind": "named",
              "name": "prodSeed",
              "path": "./src/features/db/seeds"
            }
          ]
        },
        "deployment": null,
        "emailSender": {
          "defaultFrom": {
            "email": "kitchen-sink@wasp.sh",
            "name": null
          },
          "provider": "SMTP"
        },
        "head": [
          "<link rel='manifest' href='/manifest.json' />",
          "<link rel='icon' href='/favicon.ico' />"
        ],
        "name": "KitchenSink",
        "server": {
          "envValidationSchema": {
            "alias": "serverEnvValidationSchema",
            "kind": "named",
            "name": "serverEnvValidationSchema",
            "path": "./src/env"
          },
          "middlewareConfigFn": {
            "alias": "serverMiddlewareFn",
            "kind": "named",
            "name": "serverMiddlewareFn",
            "path": "./src/serverSetup"
          },
          "onError": {
            "alias": "onServerError",
            "kind": "named",
            "name": "onServerError",
            "path": "./src/features/errors/serverOnError"
          },
          "setupFn": {
            "alias": "serverSetup",
            "kind": "named",
            "name": "serverSetup",
            "path": "./src/serverSetup"
          },
          "traceDeps": null
        },
        "title": "Wasp Kitchen Sink",
        "wasp": {
          "version": "0.27.0"
        },
        "webSocket": {
          "autoConnect": null,
          "fn": {
            "alias": "chatWebSocket",
            "kind": "named",
            "name": "chatWebSocket",
            "path": "./src/features/chat/webSocket"
          }
        }
      }
    },
    {
      "declType": "Page",
      "declValue": {
        "authRequired": null,
        "component": {
          "alias": "HomePage",
          "kind": "named",
          "name": "HomePage",
          "path": "./src/pages/HomePage"
        },
        "name": "HomePage"
      }
    },
    {
      "declType": "Route",
      "declValue": {
        "lazy": null,
        "name": "HomeRoute",
        "path": "/",
        "prerender": [
          "/"
        ],
        "to": {
          "declType": "Page",
          "name": "HomePage"
        }
      }
    },
    {
      "declType": "Page",
      "declValue": {
        "authRequired": null,
        "component": {
          "alias": "CatchAllPage",
          "kind": "named",
          "name": "CatchAllPage",
          "path": "./src/pages/CatchAllPage"
        },
        "name": "CatchAllPage"
      }
    },
    {
      "declType": "Route",
      "declValue": {
        "lazy": null,
        "name": "CatchAllRoute",
        "path": "*",
        "prerender": [],
        "to": {
          "declType": "Page",
          "name": "CatchAllPage"
        }
      }
    },
    {
      "declType": "Page",
      "declValue": {
        "authRequired": null,
        "component": {
          "alias": null,
          "kind": "default",
          "name": "Signup",
          "path": "./src/features/auth/pages/Signup"
        },
        "name": "Signup"
      }
    },
    {
      "declType": "Route",
      "declValue": {
        "lazy": null,
        "name": "SignupRoute",
        "path": "/signup",
        "prerender": [],
        "to": {
          "declType": "Page",
          "name": "Signup"
        }
      }
    },
    {
      "declType": "Page",
      "declValue": {
        "authRequired": null,
        "component": {
          "alias": null,
          "kind": "default",
          "name": "Login",
          "path": "./src/features/auth/pages/Login"
        },
        "name": "Login"
      }
    },
    {
      "declType": "Route",
      "declValue": {
        "lazy": null,
        "name": "LoginRoute",
        "path": "/login",
        "prerender": [],
        "to": {
          "declType": "Page",
          "name": "Login"
        }
      }
    },
    {
      "declType": "Page",
      "declValue": {
        "authRequired": null,
        "component": {
          "alias": "PasswordReset",
          "kind": "named",
          "name": "PasswordReset",
          "path": "./src/features/auth/pages/PasswordReset"
        },
        "name": "PasswordReset"
      }
    },
    {
      "declType": "Route",
      "declValue": {
        "lazy": null,
        "name": "PasswordResetRoute",
        "path": "/password-reset",
        "prerender": [],
        "to": {
          "declType": "Page",
          "name": "PasswordReset"
        }
      }
    },
    {
      "declType": "Page",
      "declValue": {
        "authRequired": null,
        "component": {
          "alias": "EmailVerification",
          "kind": "named",
          "name": "EmailVerification",
          "path": "./src/features/auth/pages/EmailVerification"
        },
        "name": "EmailVerification"
      }
    },
    {
      "declType": "Route",
      "declValue": {
        "lazy": null,
        "name": "EmailVerificationRoute",
        "path": "/email-verification-",
        "prerender": [],
        "to": {
          "declType": "Page",
          "name": "EmailVerification"
        }
      }
    },
    {
      "declType": "Page",
      "declValue": {
        "authRequired": null,
        "component": {
          "alias": "RequestPasswordReset",
          "kind": "named",
          "name": "RequestPasswordReset",
          "path": "./src/features/auth/pages/RequestPasswordReset"
        },
        "name": "RequestPasswordReset"
      }
    },
    {
      "declType": "Route",
      "declValue": {
        "lazy": null,
        "name": "RequestPasswordResetRoute",
        "path": "/request-password-reset",
        "prerender": [],
        "to": {
          "declType": "Page",
          "name": "RequestPasswordReset"
        }
      }
    },
    {
      "declType": "Page",
      "declValue": {
        "authRequired": true,
        "component": {
          "alias": "ProfilePage",
          "kind": "named",
          "name": "ProfilePage",
          "path": "./src/features/auth/pages/ProfilePage"
        },
        "name": "ProfilePage"
      }
    },
    {
      "declType": "Route",
      "declValue": {
        "lazy": null,
        "name": "ProfileRoute",
        "path": "/profile",
        "prerender": [],
        "to": {
          "declType": "Page",
          "name": "ProfilePage"
        }
      }
    },
    {
      "declType": "Page",
      "declValue": {
        "authRequired": null,
        "component": {
          "alias": "ManualSignupPage",
          "kind": "named",
          "name": "ManualSignupPage",
          "path": "./src/features/auth/pages/ManualSignupPage"
        },
        "name": "ManualSignupPage"
      }
    },
    {
      "declType": "Route",
      "declValue": {
        "lazy": null,
        "name": "ManualSignupRoute",
        "path": "/manual-signup",
        "prerender": [],
        "to": {
          "declType": "Page",
          "name": "ManualSignupPage"
        }
      }
    },
    {
      "declType": "Page",
      "declValue": {
        "authRequired": null,
        "component": {
          "alias": "CustomSignupPage",
          "kind": "named",
          "name": "CustomSignupPage",
          "path": "./src/features/auth/pages/CustomSignupPage"
        },
        "name": "CustomSignupPage"
      }
    },
    {
      "declType": "Route",
      "declValue": {
        "lazy": null,
        "name": "CustomSignupRoute",
        "path": "/custom-signup",
        "prerender": [],
        "to": {
          "declType": "Page",
          "name": "CustomSignupPage"
        }
      }
    },
    {
      "declType": "Action",
      "declValue": {
        "auth": null,
        "entities": null,
        "fn": {
          "alias": "customSignup",
          "kind": "named",
          "name": "customSignup",
          "path": "./src/features/auth/customSignup"
        },
        "name": "customSignup"
      }
    },
    {
      "declType": "Page",
      "declValue": {
        "authRequired": true,
        "component": {
          "alias": "TasksPage",
          "kind": "named",
          "name": "TasksPage",
          "path": "./src/features/operations/pages/TasksPage"
        },
        "name": "TasksPage"
      }
    },
    {
      "declType": "Route",
      "declValue": {
        "lazy": null,
        "name": "TasksRoute",
        "path": "/tasks",
        "prerender": [],
        "to": {
          "declType": "Page",
          "name": "TasksPage"
        }
      }
    },
    {
      "declType": "Page",
      "declValue": {
        "authRequired": true,
        "component": {
          "alias": "TaskDetailPage",
          "kind": "named",
          "name": "TaskDetailPage",
          "path": "./src/features/operations/pages/TaskDetailPage"
        },
        "name": "TaskDetailPage"
      }
    },
    {
      "declType": "Route",
      "declValue": {
        "lazy": null,
        "name": "TaskRoute",
        "path": "/tasks/:id",
        "prerender": [],
        "to": {
          "declType": "Page",
          "name": "TaskDetailPage"
        }
      }
    },
    {
      "declType": "Query",
      "declValue": {
        "auth": null,
        "entities": [
          {
            "declType": "Entity",
            "name": "Task"
          }
        ],
        "fn": {
          "alias": "getTasks",
          "kind": "named",
          "name": "getTasks",
          "path": "./src/features/operations/queries"
        },
        "name": "getTasks"
      }
    },
    {
      "declType": "Query",
      "declValue": {
        "auth": false,
        "entities": [
          {
            "declType": "Entity",
            "name": "Task"
          }
        ],
        "fn": {
          "alias": "getNumTasks",
          "kind": "named",
          "name": "getNumTasks",
          "path": "./src/features/operations/queries"
        },
        "name": "getNumTasks"
      }
    },
    {
      "declType": "Query",
      "declValue": {
        "auth": null,
        "entities": [
          {
            "declType": "Entity",
            "name": "Task"
          }
        ],
        "fn": {
          "alias": "getTask",
          "kind": "named",
          "name": "getTask",
          "path": "./src/features/operations/queries"
        },
        "name": "getTask"
      }
    },
    {
      "declType": "Query",
      "declValue": {
        "auth": null,
        "entities": [
          {
            "declType": "Entity",
            "name": "Task"
          }
        ],
        "fn": {
          "alias": null,
          "kind": "default",
          "name": "getOldestTask",
          "path": "./src/features/operations/getOldestTask"
        },
        "name": "getOldestTask"
      }
    },
    {
      "declType": "Action",
      "declValue": {
        "auth": null,
        "entities": [
          {
            "declType": "Entity",
            "name": "Task"
          }
        ],
        "fn": {
          "alias": "createTask",
          "kind": "named",
          "name": "createTask",
          "path": "./src/features/operations/actions"
        },
        "name": "createTask"
      }
    },
    {
      "declType": "Action",
      "declValue": {
        "auth": null,
        "entities": [
          {
            "declType": "Entity",
            "name": "Task"
          }
        ],
        "fn": {
          "alias": "updateTaskIsDone",
          "kind": "named",
          "name": "updateTaskIsDone",
          "path": "./src/features/operations/actions"
        },
        "name": "updateTaskIsDone"
      }
    },
    {
      "declType": "Action",
      "declValue": {
        "auth": null,
        "entities": [
          {
            "declType": "Entity",
            "name": "Task"
          }
        ],
        "fn": {
          "alias": "deleteCompletedTasks",
          "kind": "named",
          "name": "deleteCompletedTasks",
          "path": "./src/features/operations/actions"
        },
        "name": "deleteCompletedTasks"
      }
    },
    {
      "declType": "Action",
      "declValue": {
        "auth": null,
        "entities": [
          {
            "declType": "Entity",
            "name": "Task"
          }
        ],
        "fn": {
          "alias": "toggleAllTasks",
          "kind": "named",
          "name": "toggleAllTasks",
          "path": "./src/features/operations/actions"
        },
        "name": "toggleAllTasks"
      }
    },
    {
      "declType": "Query",
      "declValue": {
        "auth": null,
        "entities": null,
        "fn": {
          "alias": "getSerializedObjects",
          "kind": "named",
          "name": "getSerializedObjects",
          "path": "./src/features/operations/queries"
        },
        "name": "getSerializedObjects"
      }
    },
    {
      "declType": "Page",
      "declValue": {
        "authRequired": null,
        "component": {
          "alias": "SerializationPage",
          "kind": "named",
          "name": "SerializationPage",
          "path": "./src/features/operations/pages/SerializationPage"
        },
        "name": "SerializationPage"
      }
    },
    {
      "declType": "Route",
      "declValue": {
        "lazy": null,
        "name": "SerializationRoute",
        "path": "/serialization",
        "prerender": [],
        "to": {
          "declType": "Page",
          "name": "SerializationPage"
        }
      }
    },
    {
      "declType": "Page",
      "declValue": {
        "authRequired": true,
        "component": {
          "alias": "JobsPage",
          "kind": "named",
          "name": "JobsPage",
          "path": "./src/features/jobs/pages/JobsPage"
        },
        "name": "JobsPage"
      }
    },
    {
      "declType": "Route",
      "declValue": {
        "lazy": null,
        "name": "JobsRoute",
        "path": "/jobs",
        "prerender": [],
        "to": {
          "declType": "Page",
          "name": "JobsPage"
        }
      }
    },
    {
      "declType": "Action",
      "declValue": {
        "auth": null,
        "entities": [
          {
            "declType": "Entity",
            "name": "UppercaseTextRequest"
          }
        ],
        "fn": {
          "alias": "requestUppercaseText",
          "kind": "named",
          "name": "requestUppercaseText",
          "path": "./src/features/jobs/uppercaseText"
        },
        "name": "requestUppercaseText"
      }
    },
    {
      "declType": "Query",
      "declValue": {
        "auth": null,
        "entities": [
          {
            "declType": "Entity",
            "name": "UppercaseTextRequest"
          }
        ],
        "fn": {
          "alias": "getTextUppercaseRequests",
          "kind": "named",
          "name": "getTextUppercaseRequests",
          "path": "./src/features/jobs/uppercaseText"
        },
        "name": "getTextUppercaseRequests"
      }
    },
    {
      "declType": "Job",
      "declValue": {
        "entities": [
          {
            "declType": "Entity",
            "name": "UppercaseTextRequest"
          }
        ],
        "executor": "PgBoss",
        "name": "uppercaseTextJob",
        "perform": {
          "executorOptions": null,
          "fn": {
            "alias": "uppercaseTextJob",
            "kind": "named",
            "name": "uppercaseTextJob",
            "path": "./src/features/jobs/uppercaseText"
          }
        },
        "schedule": null
      }
    },
    {
      "declType": "Job",
      "declValue": {
        "entities": [
          {
            "declType": "Entity",
            "name": "Task"
          }
        ],
        "executor": "PgBoss",
        "name": "mySpecialJob",
        "perform": {
          "executorOptions": {
            "pgBoss": {
              "retryLimit": 1
            }
          },
          "fn": {
            "alias": "mySpecialJob",
            "kind": "named",
            "name": "mySpecialJob",
            "path": "./src/features/jobs/bar"
          }
        },
        "schedule": null
      }
    },
    {
      "declType": "Job",
      "declValue": {
        "entities": null,
        "executor": "PgBoss",
        "name": "mySpecialScheduledJob",
        "perform": {
          "executorOptions": null,
          "fn": {
            "alias": "mySpecialScheduledJob",
            "kind": "named",
            "name": "mySpecialScheduledJob",
            "path": "./src/features/jobs/bar"
          }
        },
        "schedule": {
          "args": {
            "foo": "bar"
          },
          "cron": "0 * * * *",
          "executorOptions": {
            "pgBoss": {
              "retryLimit": 2
            }
          }
        }
      }
    },
    {
      "declType": "Page",
      "declValue": {
        "authRequired": null,
        "component": {
          "alias": "ApisPage",
          "kind": "named",
          "name": "ApisPage",
          "path": "./src/features/apis/pages/ApisPage"
        },
        "name": "ApisPage"
      }
    },
    {
      "declType": "Route",
      "declValue": {
        "lazy": null,
        "name": "ApisRoute",
        "path": "/apis",
        "prerender": [],
        "to": {
          "declType": "Page",
          "name": "ApisPage"
        }
      }
    },
    {
      "declType": "Api",
      "declValue": {
        "auth": null,
        "entities": [
          {
            "declType": "Entity",
            "name": "Task"
          }
        ],
        "fn": {
          "alias": "fooBar",
          "kind": "named",
          "name": "fooBar",
          "path": "./src/features/apis/apis"
        },
        "httpRoute": [
          "ALL",
          "/foo/bar"
        ],
        "middlewareConfigFn": {
          "alias": "fooBarMiddlewareFn",
          "kind": "named",
          "name": "fooBarMiddlewareFn",
          "path": "./src/features/apis/apis"
        },
        "name": "fooBar"
      }
    },
    {
      "declType": "ApiNamespace",
      "declValue": {
        "middlewareConfigFn": {
          "alias": "barNamespaceMiddlewareFn",
          "kind": "named",
          "name": "barNamespaceMiddlewareFn",
          "path": "./src/features/apis/apis"
        },
        "name": "barNamespaceMiddlewareFn",
        "path": "/bar"
      }
    },
    {
      "declType": "Api",
      "declValue": {
        "auth": false,
        "entities": null,
        "fn": {
          "alias": "headBarBaz",
          "kind": "named",
          "name": "headBarBaz",
          "path": "./src/features/apis/apis"
        },
        "httpRoute": [
          "HEAD",
          "/bar/baz"
        ],
        "middlewareConfigFn": null,
        "name": "headBarBaz"
      }
    },
    {
      "declType": "Api",
      "declValue": {
        "auth": false,
        "entities": [
          {
            "declType": "Entity",
            "name": "Task"
          }
        ],
        "fn": {
          "alias": "barBaz",
          "kind": "named",
          "name": "barBaz",
          "path": "./src/features/apis/apis"
        },
        "httpRoute": [
          "GET",
          "/bar/baz"
        ],
        "middlewareConfigFn": null,
        "name": "barBaz"
      }
    },
    {
      "declType": "Api",
      "declValue": {
        "auth": false,
        "entities": null,
        "fn": {
          "alias": "patchBarBaz",
          "kind": "named",
          "name": "patchBarBaz",
          "path": "./src/features/apis/apis"
        },
        "httpRoute": [
          "PATCH",
          "/bar/baz"
        ],
        "middlewareConfigFn": null,
        "name": "patchBarBaz"
      }
    },
    {
      "declType": "Api",
      "declValue": {
        "auth": false,
        "entities": null,
        "fn": {
          "alias": "webhookCallback",
          "kind": "named",
          "name": "webhookCallback",
          "path": "./src/features/apis/apis"
        },
        "httpRoute": [
          "POST",
          "/webhook/callback"
        ],
        "middlewareConfigFn": {
          "alias": "webhookCallbackMiddlewareFn",
          "kind": "named",
          "name": "webhookCallbackMiddlewareFn",
          "path": "./src/features/apis/apis"
        },
        "name": "webhookCallback"
      }
    },
    {
      "declType": "Api",
      "declValue": {
        "auth": false,
        "entities": null,
        "fn": {
          "alias": "throwUnexpectedError",
          "kind": "named",
          "name": "throwUnexpectedError",
          "path": "./src/features/errors/apis"
        },
        "httpRoute": [
          "GET",
          "/errors/unexpected"
        ],
        "middlewareConfigFn": null,
        "name": "throwUnexpectedError"
      }
    },
    {
      "declType": "Api",
      "declValue": {
        "auth": false,
        "entities": null,
        "fn": {
          "alias": "throwHttpError",
          "kind": "named",
          "name": "throwHttpError",
          "path": "./src/features/errors/apis"
        },
        "httpRoute": [
          "GET",
          "/errors/http"
        ],
        "middlewareConfigFn": null,
        "name": "throwHttpError"
      }
    },
    {
      "declType": "Api",
      "declValue": {
        "auth": false,
        "entities": null,
        "fn": {
          "alias": "getReportedServerErrors",
          "kind": "named",
          "name": "getReportedServerErrors",
          "path": "./src/features/errors/serverOnError"
        },
        "httpRoute": [
          "GET",
          "/errors/reported"
        ],
        "middlewareConfigFn": null,
        "name": "getReportedServerErrors"
      }
    },
    {
      "declType": "Query",
      "declValue": {
        "auth": false,
        "entities": null,
        "fn": {
          "alias": "getTeapot",
          "kind": "named",
          "name": "getTeapot",
          "path": "./src/features/errors/queries"
        },
        "name": "getTeapot"
      }
    },
    {
      "declType": "Page",
      "declValue": {
        "authRequired": null,
        "component": {
          "alias": "ErrorsPage",
          "kind": "named",
          "name": "ErrorsPage",
          "path": "./src/features/errors/pages/ErrorsPage"
        },
        "name": "ErrorsPage"
      }
    },
    {
      "declType": "Route",
      "declValue": {
        "lazy": null,
        "name": "ErrorsRoute",
        "path": "/errors",
        "prerender": [],
        "to": {
          "declType": "Page",
          "name": "ErrorsPage"
        }
      }
    },
    {
      "declType": "Page",
      "declValue": {
        "authRequired": null,
        "component": {
          "alias": "ClientErrorsPage",
          "kind": "named",
          "name": "ClientErrorsPage",
          "path": "./src/features/errors/pages/ClientErrorsPage"
        },
        "name": "ClientErrorsPage"
      }
    },
    {
      "declType": "Route",
      "declValue": {
        "lazy": null,
        "name": "ClientErrorsRoute",
        "path": "/errors/client",
        "prerender": [],
        "to": {
          "declType": "Page",
          "name": "ClientErrorsPage"
        }
      }
    },
    {
      "declType": "Crud",
      "declValue": {
        "entity": {
          "declType": "Entity",
          "name": "Task"
        },
        "name": "tasks",
        "operations": {
          "create": {
            "isPublic": null,
            "overrideFn": {
              "alias": "crudCreateTask",
              "kind": "named",
              "name": "crudCreateTask",
              "path": "./src/features/crud/crud"
            }
          },
          "delete": {
            "isPublic": null,
            "overrideFn": null
          },
          "get": {
            "isPublic": null,
            "overrideFn": null
          },
          "getAll": {
            "isPublic": null,
            "overrideFn": {
              "alias": "crudGetAllTasks",
              "kind": "named",
              "name": "crudGetAllTasks",
              "path": "./src/features/crud/crud"
            }
          },
          "update": {
            "isPublic": null,
            "overrideFn": null
          }
        }
      }
    },
    {
      "declType": "Crud",
      "declValue": {
        "entity": {
          "declType": "Entity",
          "name": "TaskVote"
        },
        "name": "taskVotes",
        "operations": {
          "create": null,
          "delete": null,
          "get": null,
          "getAll": {
            "isPublic": null,
            "overrideFn": null
          },
          "update": null
        }
      }
    },
    {
      "declType": "Page",
      "declValue": {
        "authRequired": true,
        "component": {
          "alias": "ListPage",
          "kind": "named",
          "name": "ListPage",
          "path": "./src/features/crud/pages/ListPage"
        },
        "name": "ListPage"
      }
    },
    {
      "declType": "Route",
      "declValue": {
        "lazy": null,
        "name": "CrudListRoute",
        "path": "/crud",
        "prerender": [],
        "to": {
          "declType": "Page",
          "name": "ListPage"
        }
      }
    },
    {
      "declType": "Page",
      "declValue": {
        "authRequired": true,
        "component": {
          "alias": "DetailPage",
          "kind": "named",
          "name": "DetailPage",
          "path": "./src/features/crud/pages/DetailPage"
        },
        "name": "DetailPage"
      }
    },
    {
      "declType": "Route",
      "declValue": {
        "lazy": null,
        "name": "CrudDetailRoute",
        "path": "/crud/:id",
        "prerender": [],
        "to": {
          "declType": "Page",
          "name": "DetailPage"
        }
      }
    },
    {
      "declType": "Api",
      "declValue": {
        "auth": null,
        "entities": null,
        "fn": {
          "alias": "streamingText",
          "kind": "named",
          "name": "streamingText",
          "path": "./src/features/streaming/api"
        },
        "httpRoute": [
          "GET",
          "/api/streaming-test"
        ],
        "middlewareConfigFn": null,
        "name": "streamingText"
      }
    },
    {
      "declType": "ApiNamespace",
      "declValue": {
        "middlewareConfigFn": {
          "alias": "defaultMiddlewareForStreamingText",
          "kind": "named",
          "name": "defaultMiddlewareForStreamingText",
          "path": "./src/features/streaming/api"
        },
        "name": "defaultMiddlewareForStreamingText",
        "path": "/api/streaming-test"
      }
    },
    {
      "declType": "Page",
      "declValue": {
        "authRequired": null,
        "component": {
          "alias": "StreamingTestPage",
          "kind": "named",
          "name": "StreamingTestPage",
          "path": "./src/features/streaming/pages/StreamingTestPage"
        },
        "name": "StreamingTestPage"
      }
    },
    {
      "declType": "Route",
      "declValue": {
        "lazy": null,
        "name": "StreamingRoute",
        "path": "/streaming",
        "prerender": [],
        "to": {
          "declType": "Page",
          "name": "StreamingTestPage"
        }
      }
    },
    {
      "declType": "Page",
      "declValue": {
        "authRequired": true,
        "component": {
          "alias": "ChatPage",
          "kind": "named",
          "name": "ChatPage",
          "path": "./src/features/chat/pages/ChatPage"
        },
        "name": "ChatPage"
      }
    },
    {
      "declType": "Route",
      "declValue": {
        "lazy": null,
        "name": "ChatRoute",
        "path": "/chat",
        "prerender": [],
        "to": {
          "declType": "Page",
          "name": "ChatPage"
        }
      }
    },
    {
      "declType": "Page",
      "declValue": {
        "authRequired": null,
        "component": {
          "alias": "EagerPage",
          "kind": "named",
          "name": "EagerPage",
          "path": "./src/features/lazy-loading/pages/EagerPage"
        },
        "name": "EagerPage"
      }
    },
    {
      "declType": "Route",
      "declValue": {
        "lazy": false,
        "name": "EagerRoute",
        "path": "/lazy/no",
        "prerender": [],
        "to": {
          "declType": "Page",
          "name": "EagerPage"
        }
      }
    },
    {
      "declType": "Page",
      "declValue": {
        "authRequired": null,
        "component": {
          "alias": "LazyPage",
          "kind": "named",
          "name": "LazyPage",
          "path": "./src/features/lazy-loading/pages/LazyPage"
        },
        "name": "LazyPage"
      }
    },
    {
      "declType": "Route",
      "declValue": {
        "lazy": null,
        "name": "LazyRoute",
        "path": "/lazy/yes",
        "prerender": [],
        "to": {
          "declType": "Page",
          "name": "LazyPage"
        }
      }
    },
    {
      "declType": "Page",
      "declValue": {
        "authRequired": null,
        "component": {
          "alias": "PrerenderPage",
          "kind": "named",
          "name": "PrerenderPage",
          "path": "./src/features/prerender/pages/PrerenderPage"
        },
        "name": "PrerenderPage"
      }
    },
    {
      "declType": "Route",
      "declValue": {
        "lazy": null,
        "name": "PrerenderRoute",
        "path": "/prerender",
        "prerender": [
          "/prerender"
        ],
        "to": {
          "declType": "Page",
          "name": "PrerenderPage"
        }
      }
    },
    {
      "declType": "Page",
      "declValue": {
        "authRequired": null,
        "component": {
          "alias": "PrerenderInstancesPage",
          "kind": "named",
          "name": "PrerenderInstancesPage",
          "path": "./src/features/prerender/pages/PrerenderInstancesPage"
        },
        "name": "PrerenderInstancesPage"
      }
    },
    {
      "declType": "Route",
      "declValue": {
        "lazy": null,
        "name": "PrerenderInstancesRoute",
        "path": "/prerender-instances/:slug",
        "prerender": [
          "/prerender-instances/martin",
          "/prerender-instances/matija"
        ],
        "to": {
          "declType": "Page",
          "name": "PrerenderInstancesPage"
        }
      }
    },
    {
      "declType": "Page",
      "declValue": {
        "authRequired": null,
        "component": {
          "alias": "HydrationMismatchPage",
          "kind": "named",
          "name": "HydrationMismatchPage",
          "path": "./src/features/prerender/pages/HydrationMismatchPage"
        },
        "name": "HydrationMismatchPage"
      }
    },
    {
      "declType": "Route",
      "declValue": {
        "lazy": null,
        "name": "HydrationMismatchRoute",
        "path": "/hydration-mismatch",
        "prerender": [
          "/hydration-mismatch"
        ],
        "to": {
          "declType": "Page",
          "name": "HydrationMismatchPage"
        }
      }
    },
    {
      "declType": "Action",
      "declValue": {
        "auth": null,
        "entities": [],
        "fn": {
          "alias": "testingAction",
          "kind": "named",
          "name": "testingAction",
          "path": "./src/rpcTests/operations/server"
        },
        "name": "testingAction"
      }
    },
    {
      "declType": "Query",
      "declValue": {
        "auth": null,
        "entities": null,
        "fn": {
          "alias": "getDate",
          "kind": "named",
          "name": "getDate",
          "path": "./src/rpcTests/operations/definitions"
        },
        "name": "getDate"
      }
    },
    {
      "declType": "Query",
      "declValue": {
        "auth": false,
        "entities": [],
        "fn": {
          "alias": "getAnythingNoAuth",
          "kind": "named",
          "name": "getAnythingNoAuth",
          "path": "./src/rpcTests/operations/definitions"
        },
        "name": "getAnythingNoAuth"
      }
    },
    {
      "declType": "Query",
      "declValue": {
        "auth": true,
        "entities": [],
        "fn": {
          "alias": "getAnythingAuth",
          "kind": "named",
          "name": "getAnythingAuth",
          "path": "./src/rpcTests/operations/definitions"
        },
        "name": "getAnythingAuth"
      }
    },
    {
      "declType": "Query",
      "declValue": {
        "auth": null,
        "entities": [],
        "fn": {
          "alias": "getTrueVoid",
          "kind": "named",
          "name": "getTrueVoid",
          "path": "./src/rpcTests/operations/definitions"
        },
        "name": "getTrueVoid"
      }
    },
    {
      "declType": "Query",
      "declValue": {
        "auth": false,
        "entities": [],
        "fn": {
          "alias": "getAnyNoAuth",
          "kind": "named",
          "name": "getAnyNoAuth",
          "path": "./src/rpcTests/operations/definitions"
        },
        "name": "getAnyNoAuth"
      }
    },
    {
      "declType": "Query",
      "declValue": {
        "auth": true,
        "entities": [],
        "fn": {
          "alias": "getAnyAuth",
          "kind": "named",
          "name": "getAnyAuth",
          "path": "./src/rpcTests/operations/definitions"
        },
        "name": "getAnyAuth"
      }
    },
    {
      "declType": "Query",
      "declValue": {
        "auth": true,
        "entities": [],
        "fn": {
          "alias": "getAnyToNumberSpecified",
          "kind": "named",
          "name": "getAnyToNumberSpecified",
          "path": "./src/rpcTests/operations/definitions"
        },
        "name": "getAnyToNumberSpecified"
      }
    },
    {
      "declType": "Action",
      "declValue": {
        "auth": null,
        "entities": [
          {
            "declType": "Entity",
            "name": "Task"
          }
        ],
        "fn": {
          "alias": "taskToTaskUnspecified",
          "kind": "named",
          "name": "taskToTaskUnspecified",
          "path": "./src/rpcTests/operations/definitions"
        },
        "name": "taskToTaskUnspecified"
      }
    },
    {
      "declType": "Action",
      "declValue": {
        "auth": null,
        "entities": [
          {
            "declType": "Entity",
            "name": "Task"
          }
        ],
        "fn": {
          "alias": "taskToTaskSatisfies",
          "kind": "named",
          "name": "taskToTaskSatisfies",
          "path": "./src/rpcTests/operations/definitions"
        },
        "name": "taskToTaskSatisfies"
      }
    },
    {
      "declType": "Action",
      "declValue": {
        "auth": null,
        "entities": [
          {
            "declType": "Entity",
            "name": "Task"
          }
        ],
        "fn": {
          "alias": "taskToTaskSpecified",
          "kind": "named",
          "name": "taskToTaskSpecified",
          "path": "./src/rpcTests/operations/definitions"
        },
        "name": "taskToTaskSpecified"
      }
    },
    {
      "declType": "Action",
      "declValue": {
        "auth": true,
        "entities": [
          {
            "declType": "Entity",
            "name": "Task"
          }
        ],
        "fn": {
          "alias": "voidToStringAuth",
          "kind": "named",
          "name": "voidToStringAuth",
          "path": "./src/rpcTests/operations/definitions"
        },
        "name": "voidToStringAuth"
      }
    },
    {
      "declType": "Action",
      "declValue": {
        "auth": false,
        "entities": [
          {
            "declType": "Entity",
            "name": "Task"
          }
        ],
        "fn": {
          "alias": "voidToStringNoAuth",
          "kind": "named",
          "name": "voidToStringNoAuth",
          "path": "./src/rpcTests/operations/definitions"
        },
        "name": "voidToStringNoAuth"
      }
    },
    {
      "declType": "Action",
      "declValue": {
        "auth": null,
        "entities": [
          {
            "declType": "Entity",
            "name": "Task"
          }
        ],
        "fn": {
          "alias": "unspecifiedToNumber",
          "kind": "named",
          "name": "unspecifiedToNumber",
          "path": "./src/rpcTests/operations/definitions"
        },
        "name": "unspecifiedToNumber"
      }
    },
    {
      "declType": "Action",
      "declValue": {
        "auth": true,
        "entities": [
          {
            "declType": "Entity",
            "name": "Task"
          }
        ],
        "fn": {
          "alias": "boolToStringAuth",
          "kind": "named",
          "name": "boolToStringAuth",
          "path": "./src/rpcTests/operations/definitions"
        },
        "name": "boolToStringAuth"
      }
    },
    {
      "declType": "Action",
      "declValue": {
        "auth": false,
        "entities": [
          {
            "declType": "Entity",
            "name": "Task"
          }
        ],
        "fn": {
          "alias": "boolToStringNoAuth",
          "kind": "named",
          "name": "boolToStringNoAuth",
          "path": "./src/rpcTests/operations/definitions"
        },
        "name": "boolToStringNoAuth"
      }
    },
    {
      "declType": "Action",
      "declValue": {
        "auth": false,
        "entities": [
          {
            "declType": "Entity",
            "name": "Task"
          }
        ],
        "fn": {
          "alias": "boolToVoidNoAuth",
          "kind": "named",
          "name": "boolToVoidNoAuth",
          "path": "./src/rpcTests/operations/definitions"
        },
        "name": "boolToVoidNoAuth"
      }
    },
    {
      "declType": "Action",
      "declValue": {
        "auth": true,
        "entities": [
          {
            "declType": "Entity",
            "name": "Task"
          }
        ],
        "fn": {
          "alias": "boolToVoidAuth",
          "kind": "named",
          "name": "boolToVoidAuth",
          "path": "./src/rpcTests/operations/definitions"
        },
        "name": "boolToVoidAuth"
      }
    },
    {
      "declType": "Action",
      "declValue": {
        "auth": true,
        "entities": [
          {
            "declType": "Entity",
            "name": "Task"
          }
        ],
        "fn": {
          "alias": "jsActionWithArgs",
          "kind": "named",
          "name": "jsActionWithArgs",
          "path": "./src/rpcTests/operations/jsDefinitions"
        },
        "name": "jsActionWithArgs"
      }
    }
  ],
  "waspVersion": "0.27.0"
};
