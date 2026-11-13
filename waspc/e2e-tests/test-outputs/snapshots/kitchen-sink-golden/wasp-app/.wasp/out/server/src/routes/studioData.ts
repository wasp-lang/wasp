// The data that Wasp Studio shows, from the app's Wasp config. Only generated
// in development, since it describes the app's internals.
export const studioData = {
  "apis": [
    {
      "auth": null,
      "entities": [
        {
          "name": "Task"
        }
      ],
      "httpRoute": {
        "method": "ALL",
        "path": "/foo/bar"
      },
      "name": "fooBar"
    },
    {
      "auth": false,
      "entities": [],
      "httpRoute": {
        "method": "HEAD",
        "path": "/bar/baz"
      },
      "name": "headBarBaz"
    },
    {
      "auth": false,
      "entities": [
        {
          "name": "Task"
        }
      ],
      "httpRoute": {
        "method": "GET",
        "path": "/bar/baz"
      },
      "name": "barBaz"
    },
    {
      "auth": false,
      "entities": [],
      "httpRoute": {
        "method": "PATCH",
        "path": "/bar/baz"
      },
      "name": "patchBarBaz"
    },
    {
      "auth": false,
      "entities": [],
      "httpRoute": {
        "method": "POST",
        "path": "/webhook/callback"
      },
      "name": "webhookCallback"
    },
    {
      "auth": false,
      "entities": [],
      "httpRoute": {
        "method": "GET",
        "path": "/errors/unexpected"
      },
      "name": "throwUnexpectedError"
    },
    {
      "auth": false,
      "entities": [],
      "httpRoute": {
        "method": "GET",
        "path": "/errors/http"
      },
      "name": "throwHttpError"
    },
    {
      "auth": false,
      "entities": [],
      "httpRoute": {
        "method": "GET",
        "path": "/errors/concealed"
      },
      "name": "throwConcealedError"
    },
    {
      "auth": false,
      "entities": [],
      "httpRoute": {
        "method": "GET",
        "path": "/errors/unavailable"
      },
      "name": "throwUnavailableError"
    },
    {
      "auth": false,
      "entities": [],
      "httpRoute": {
        "method": "GET",
        "path": "/errors/rate-limit"
      },
      "name": "throwRateLimitError"
    },
    {
      "auth": false,
      "entities": [],
      "httpRoute": {
        "method": "GET",
        "path": "/errors/upstream"
      },
      "name": "throwUpstreamError"
    },
    {
      "auth": null,
      "entities": [],
      "httpRoute": {
        "method": "GET",
        "path": "/api/streaming-test"
      },
      "name": "streamingText"
    }
  ],
  "app": {
    "auth": {
      "methods": [
        "slack",
        "discord",
        "google",
        "gitHub",
        "email"
      ],
      "userEntity": {
        "name": "User"
      }
    },
    "db": {
      "system": "PostgreSQL"
    },
    "name": "KitchenSink"
  },
  "entities": [
    {
      "name": "User"
    },
    {
      "name": "Task"
    },
    {
      "name": "TaskVote"
    },
    {
      "name": "UppercaseTextRequest"
    }
  ],
  "jobs": [
    {
      "entities": [
        {
          "name": "UppercaseTextRequest"
        }
      ],
      "name": "uppercaseTextJob",
      "schedule": null
    },
    {
      "entities": [
        {
          "name": "Task"
        }
      ],
      "name": "mySpecialJob",
      "schedule": null
    },
    {
      "entities": [],
      "name": "mySpecialScheduledJob",
      "schedule": "0 * * * *"
    }
  ],
  "operations": [
    {
      "auth": null,
      "entities": [
        {
          "name": "Task"
        }
      ],
      "name": "getTasks",
      "type": "query"
    },
    {
      "auth": false,
      "entities": [
        {
          "name": "Task"
        }
      ],
      "name": "getNumTasks",
      "type": "query"
    },
    {
      "auth": null,
      "entities": [
        {
          "name": "Task"
        }
      ],
      "name": "getTask",
      "type": "query"
    },
    {
      "auth": null,
      "entities": [
        {
          "name": "Task"
        }
      ],
      "name": "getOldestTask",
      "type": "query"
    },
    {
      "auth": null,
      "entities": [],
      "name": "getSerializedObjects",
      "type": "query"
    },
    {
      "auth": null,
      "entities": [
        {
          "name": "UppercaseTextRequest"
        }
      ],
      "name": "getTextUppercaseRequests",
      "type": "query"
    },
    {
      "auth": null,
      "entities": [],
      "name": "getDate",
      "type": "query"
    },
    {
      "auth": false,
      "entities": [],
      "name": "getAnythingNoAuth",
      "type": "query"
    },
    {
      "auth": true,
      "entities": [],
      "name": "getAnythingAuth",
      "type": "query"
    },
    {
      "auth": null,
      "entities": [],
      "name": "getTrueVoid",
      "type": "query"
    },
    {
      "auth": false,
      "entities": [],
      "name": "getAnyNoAuth",
      "type": "query"
    },
    {
      "auth": true,
      "entities": [],
      "name": "getAnyAuth",
      "type": "query"
    },
    {
      "auth": true,
      "entities": [],
      "name": "getAnyToNumberSpecified",
      "type": "query"
    },
    {
      "auth": null,
      "entities": [],
      "name": "customSignup",
      "type": "action"
    },
    {
      "auth": null,
      "entities": [
        {
          "name": "Task"
        }
      ],
      "name": "createTask",
      "type": "action"
    },
    {
      "auth": null,
      "entities": [
        {
          "name": "Task"
        }
      ],
      "name": "updateTaskIsDone",
      "type": "action"
    },
    {
      "auth": null,
      "entities": [
        {
          "name": "Task"
        }
      ],
      "name": "deleteCompletedTasks",
      "type": "action"
    },
    {
      "auth": null,
      "entities": [
        {
          "name": "Task"
        }
      ],
      "name": "toggleAllTasks",
      "type": "action"
    },
    {
      "auth": null,
      "entities": [
        {
          "name": "UppercaseTextRequest"
        }
      ],
      "name": "requestUppercaseText",
      "type": "action"
    },
    {
      "auth": null,
      "entities": [],
      "name": "testingAction",
      "type": "action"
    },
    {
      "auth": null,
      "entities": [
        {
          "name": "Task"
        }
      ],
      "name": "taskToTaskUnspecified",
      "type": "action"
    },
    {
      "auth": null,
      "entities": [
        {
          "name": "Task"
        }
      ],
      "name": "taskToTaskSatisfies",
      "type": "action"
    },
    {
      "auth": null,
      "entities": [
        {
          "name": "Task"
        }
      ],
      "name": "taskToTaskSpecified",
      "type": "action"
    },
    {
      "auth": true,
      "entities": [
        {
          "name": "Task"
        }
      ],
      "name": "voidToStringAuth",
      "type": "action"
    },
    {
      "auth": false,
      "entities": [
        {
          "name": "Task"
        }
      ],
      "name": "voidToStringNoAuth",
      "type": "action"
    },
    {
      "auth": null,
      "entities": [
        {
          "name": "Task"
        }
      ],
      "name": "unspecifiedToNumber",
      "type": "action"
    },
    {
      "auth": true,
      "entities": [
        {
          "name": "Task"
        }
      ],
      "name": "boolToStringAuth",
      "type": "action"
    },
    {
      "auth": false,
      "entities": [
        {
          "name": "Task"
        }
      ],
      "name": "boolToStringNoAuth",
      "type": "action"
    },
    {
      "auth": false,
      "entities": [
        {
          "name": "Task"
        }
      ],
      "name": "boolToVoidNoAuth",
      "type": "action"
    },
    {
      "auth": true,
      "entities": [
        {
          "name": "Task"
        }
      ],
      "name": "boolToVoidAuth",
      "type": "action"
    },
    {
      "auth": true,
      "entities": [
        {
          "name": "Task"
        }
      ],
      "name": "jsActionWithArgs",
      "type": "action"
    }
  ],
  "pages": [
    {
      "authRequired": null,
      "name": "HomePage"
    },
    {
      "authRequired": null,
      "name": "CatchAllPage"
    },
    {
      "authRequired": null,
      "name": "Signup"
    },
    {
      "authRequired": null,
      "name": "Login"
    },
    {
      "authRequired": null,
      "name": "PasswordReset"
    },
    {
      "authRequired": null,
      "name": "EmailVerification"
    },
    {
      "authRequired": null,
      "name": "RequestPasswordReset"
    },
    {
      "authRequired": true,
      "name": "ProfilePage"
    },
    {
      "authRequired": null,
      "name": "ManualSignupPage"
    },
    {
      "authRequired": null,
      "name": "CustomSignupPage"
    },
    {
      "authRequired": true,
      "name": "TasksPage"
    },
    {
      "authRequired": true,
      "name": "TaskDetailPage"
    },
    {
      "authRequired": null,
      "name": "SerializationPage"
    },
    {
      "authRequired": true,
      "name": "JobsPage"
    },
    {
      "authRequired": null,
      "name": "ApisPage"
    },
    {
      "authRequired": true,
      "name": "ListPage"
    },
    {
      "authRequired": true,
      "name": "DetailPage"
    },
    {
      "authRequired": null,
      "name": "StreamingTestPage"
    },
    {
      "authRequired": true,
      "name": "ChatPage"
    },
    {
      "authRequired": null,
      "name": "EagerPage"
    },
    {
      "authRequired": null,
      "name": "LazyPage"
    },
    {
      "authRequired": null,
      "name": "PrerenderPage"
    },
    {
      "authRequired": null,
      "name": "PrerenderInstancesPage"
    },
    {
      "authRequired": null,
      "name": "HydrationMismatchPage"
    }
  ],
  "routes": [
    {
      "name": "HomeRoute",
      "path": "/",
      "toPage": {
        "name": "HomePage"
      }
    },
    {
      "name": "CatchAllRoute",
      "path": "*",
      "toPage": {
        "name": "CatchAllPage"
      }
    },
    {
      "name": "SignupRoute",
      "path": "/signup",
      "toPage": {
        "name": "Signup"
      }
    },
    {
      "name": "LoginRoute",
      "path": "/login",
      "toPage": {
        "name": "Login"
      }
    },
    {
      "name": "PasswordResetRoute",
      "path": "/password-reset",
      "toPage": {
        "name": "PasswordReset"
      }
    },
    {
      "name": "EmailVerificationRoute",
      "path": "/email-verification-",
      "toPage": {
        "name": "EmailVerification"
      }
    },
    {
      "name": "RequestPasswordResetRoute",
      "path": "/request-password-reset",
      "toPage": {
        "name": "RequestPasswordReset"
      }
    },
    {
      "name": "ProfileRoute",
      "path": "/profile",
      "toPage": {
        "name": "ProfilePage"
      }
    },
    {
      "name": "ManualSignupRoute",
      "path": "/manual-signup",
      "toPage": {
        "name": "ManualSignupPage"
      }
    },
    {
      "name": "CustomSignupRoute",
      "path": "/custom-signup",
      "toPage": {
        "name": "CustomSignupPage"
      }
    },
    {
      "name": "TasksRoute",
      "path": "/tasks",
      "toPage": {
        "name": "TasksPage"
      }
    },
    {
      "name": "TaskRoute",
      "path": "/tasks/:id",
      "toPage": {
        "name": "TaskDetailPage"
      }
    },
    {
      "name": "SerializationRoute",
      "path": "/serialization",
      "toPage": {
        "name": "SerializationPage"
      }
    },
    {
      "name": "JobsRoute",
      "path": "/jobs",
      "toPage": {
        "name": "JobsPage"
      }
    },
    {
      "name": "ApisRoute",
      "path": "/apis",
      "toPage": {
        "name": "ApisPage"
      }
    },
    {
      "name": "CrudListRoute",
      "path": "/crud",
      "toPage": {
        "name": "ListPage"
      }
    },
    {
      "name": "CrudDetailRoute",
      "path": "/crud/:id",
      "toPage": {
        "name": "DetailPage"
      }
    },
    {
      "name": "StreamingRoute",
      "path": "/streaming",
      "toPage": {
        "name": "StreamingTestPage"
      }
    },
    {
      "name": "ChatRoute",
      "path": "/chat",
      "toPage": {
        "name": "ChatPage"
      }
    },
    {
      "name": "EagerRoute",
      "path": "/lazy/no",
      "toPage": {
        "name": "EagerPage"
      }
    },
    {
      "name": "LazyRoute",
      "path": "/lazy/yes",
      "toPage": {
        "name": "LazyPage"
      }
    },
    {
      "name": "PrerenderRoute",
      "path": "/prerender",
      "toPage": {
        "name": "PrerenderPage"
      }
    },
    {
      "name": "PrerenderInstancesRoute",
      "path": "/prerender-instances/:slug",
      "toPage": {
        "name": "PrerenderInstancesPage"
      }
    },
    {
      "name": "HydrationMismatchRoute",
      "path": "/hydration-mismatch",
      "toPage": {
        "name": "HydrationMismatchPage"
      }
    }
  ]
};
