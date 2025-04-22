import * as UserSpec from '../userApi.js'

// Contains sample data that can be used for testing purposes.
// In our case the sample data represents UserSpec data.
// Further explanation:
// https://stackoverflow.com/questions/12071344/what-are-fixtures-in-programming

export const TASK_ENTITY = 'Task'
export const USER_ENTITY = 'User'
export const SOCIAL_USER_ENTITY = 'SocialUser'
export const ENTITIES = [TASK_ENTITY, USER_ENTITY, SOCIAL_USER_ENTITY]

export const APP = {
  NAME: 'todoApp',
  CONFIG: {
    title: 'ToDO App',
    wasp: { version: '^0.16.0' },
    head: ['<link rel="icon" href="/favicon.ico" />'],
  } satisfies Required<UserSpec.AppConfig>,
} as const

export const CRUD = {
  NAME: 'TaskCrud',
  CONFIG: {
    entity: TASK_ENTITY,
    operations: {
      get: {
        isPublic: true,
        overrideFn: {
          import: 'getTask',
          from: '@src/cruds/task',
        },
      },
      getAll: {
        isPublic: true,
      },
      create: {
        isPublic: false,
      },
      update: {
        isPublic: false,
      },
      delete: {
        isPublic: false,
        overrideFn: {
          import: 'deleteTask',
          from: '@src/cruds/task',
        },
      },
    },
  } satisfies Required<UserSpec.Crud>,
} as const

export const PAGE = {
  LOGIN: {
    NAME: 'LoginPage',
    CONFIG: {
      component: {
        from: '@src/pages/auth/Login',
        import: 'Login',
      },
      authRequired: false,
    } satisfies Required<UserSpec.PageConfig>,
  },
  EMAIL_VERIFICATION: {
    NAME: 'EmailVerificationPage',
    CONFIG: {
      component: {
        from: '@src/pages/auth/EmailVerification',
        import: 'EmailVerification',
      },
      authRequired: false,
    } satisfies Required<UserSpec.PageConfig>,
  },
  PASSWORD_RESET: {
    NAME: 'PasswordResetPage',
    CONFIG: {
      component: {
        from: '@src/pages/auth/PasswordReset',
        import: 'PasswordReset',
      },
      authRequired: false,
    } satisfies Required<UserSpec.PageConfig>,
  },
} as const

// For simplicity sake we asserted `RouteConfig.to` as branded type
// instead of creating a function which would accept branded string.
export const ROUTE = {
  LOGIN: {
    NAME: 'LoginRoute',
    CONFIG: {
      path: '/login',
      to: PAGE.LOGIN.NAME as string & { _brand: 'Page' },
    } satisfies Required<UserSpec.RouteConfig>,
  },
  EMAIL_VERIFICATION: {
    NAME: 'EmailVerificationRoute',
    CONFIG: {
      path: '/email-verification',
      to: PAGE.EMAIL_VERIFICATION.NAME as string & { _brand: 'Page' },
    } satisfies Required<UserSpec.RouteConfig>,
  },
  PASSWORD_RESET: {
    NAME: 'PasswordResetRoute',
    CONFIG: {
      path: '/password-reset',
      to: PAGE.PASSWORD_RESET.NAME as string & { _brand: 'Page' },
    } satisfies Required<UserSpec.RouteConfig>,
  },
} as const

export const API_NAMESPACE = {
  NAME: 'bar',
  CONFIG: {
    middlewareConfigFn: {
      import: 'barMiddlewareConfigFn',
      from: '@src/apis',
    },
    path: '/bar',
  } satisfies Required<UserSpec.ApiNamespaceConfig>,
} as const

export const API = {
  NAME: 'barBaz',
  CONFIG: {
    fn: {
      import: 'barBaz',
      from: '@src/apis',
    },
    auth: true,
    httpRoute: {
      method: 'GET',
      route: '/bar/baz',
    },
    entities: [TASK_ENTITY],
    middlewareConfigFn: {
      import: 'barBazMiddlewareConfigFn',
      from: '@src/apis',
    },
  } satisfies Required<UserSpec.ApiConfig>,
} as const

export const ACTION = {
  NAME: 'createTask',
  CONFIG: {
    fn: {
      import: 'createTask',
      from: '@src/actions',
    },
    entities: [TASK_ENTITY],
    auth: true,
  } satisfies Required<UserSpec.ActionConfig>,
} as const

export const QUERY = {
  NAME: 'getTasks',
  CONFIG: {
    fn: {
      import: 'getTasks',
      from: '@src/queries',
    },
    entities: [TASK_ENTITY],
    auth: true,
  } satisfies Required<UserSpec.QueryConfig>,
} as const

export const JOB = {
  NAME: 'mySpecialJob',
  CONFIG: {
    executor: 'PgBoss',
    perform: {
      fn: {
        import: 'perform',
        from: '@src/jobs/bar',
      },
    },
    entities: [TASK_ENTITY],
    schedule: {
      cron: '0 0 * * *',
      args: { foo: 'bar' },
      executorOptions: {
        pgBoss: { jobOptions: { attempts: 3 } },
      },
    },
  } satisfies Required<UserSpec.JobConfig>,
} as const

export const AUTH = {
  CONFIG: {
    userEntity: USER_ENTITY,
    methods: {
      discord: {
        configFn: {
          import: 'config',
          from: '@src/auth/discord',
        },
        userSignupFields: {
          import: 'userSignupFields',
          from: '@src/auth/discord',
        },
      },
      google: {
        configFn: {
          import: 'config',
          from: '@src/auth/google',
        },
        userSignupFields: {
          import: 'userSignupFields',
          from: '@src/auth/google',
        },
      },
      gitHub: {
        configFn: {
          import: 'config',
          from: '@src/auth/github',
        },
        userSignupFields: {
          import: 'userSignupFields',
          from: '@src/auth/github',
        },
      },
      keycloak: {
        configFn: {
          import: 'config',
          from: '@src/auth/keycloak',
        },
        userSignupFields: {
          import: 'userSignupFields',
          from: '@src/auth/keycloak',
        },
      },
      usernameAndPassword: {
        userSignupFields: {
          import: 'userSignupFields',
          from: '@src/auth/usernameAndPassword',
        },
      },
      email: {
        userSignupFields: {
          import: 'userSignupFields',
          from: '@src/auth/email',
        },
        fromField: {
          name: 'ToDo App',
          email: 'test@domain.tld',
        },
        emailVerification: {
          getEmailContentFn: {
            import: 'getVerificationEmailContent',
            from: '@src/auth/email',
          },
          clientRoute: ROUTE.EMAIL_VERIFICATION.NAME,
        },
        passwordReset: {
          getEmailContentFn: {
            import: 'getPasswordResetEmailContent',
            from: '@src/auth/email',
          },
          clientRoute: ROUTE.PASSWORD_RESET.NAME,
        },
      },
    },
    onAuthFailedRedirectTo: '/login',
    onAuthSucceededRedirectTo: '/profile',
    onBeforeSignup: {
      import: 'onBeforeSignup',
      from: '@src/auth/hooks.js',
    },
    onAfterSignup: {
      import: 'onAfterSignup',
      from: '@src/auth/hooks.js',
    },
    onBeforeOAuthRedirect: {
      import: 'onBeforeOAuthRedirect',
      from: '@src/auth/hooks.js',
    },
    onBeforeLogin: {
      import: 'onBeforeLogin',
      from: '@src/auth/hooks.js',
    },
    onAfterLogin: {
      import: 'onAfterLogin',
      from: '@src/auth/hooks.js',
    },
    externalAuthEntity: SOCIAL_USER_ENTITY,
  } satisfies Required<UserSpec.AuthConfig>,
} as const

export const CLIENT = {
  CONFIG: {
    rootComponent: {
      from: '@src/App',
      import: 'App',
    },
    setupFn: {
      from: '@src/clientSetup',
      import: 'setup',
    },
    baseDir: '/src',
    envValidationSchema: {
      import: 'envValidationSchema',
      from: '@src/envValidationSchema',
    },
  } satisfies Required<UserSpec.ClientConfig>,
} as const

export const SERVER = {
  CONFIG: {
    setupFn: {
      import: 'setup',
      from: '@src/serverSetup',
    },
    middlewareConfigFn: {
      import: 'serverMiddlewareFn',
      from: '@src/serverSetup',
    },
    envValidationSchema: {
      import: 'envValidationSchema',
      from: '@src/envValidationSchema',
    },
  } satisfies Required<UserSpec.ServerConfig>,
} as const

export const WEBSOCKET = {
  CONFIG: {
    fn: {
      import: 'webSocketFn',
      from: '@src/webSocket',
    },
    autoConnect: true,
  } satisfies Required<UserSpec.WebsocketConfig>,
} as const

export const DB = {
  CONFIG: {
    seeds: [
      {
        import: 'devSeedSimple',
        from: '@src/dbSeeds',
      },
    ],
  } satisfies Required<UserSpec.DbConfig>,
} as const

export const EMAIL = {
  CONFIG: {
    provider: 'SMTP',
    defaultFrom: {
      name: 'Test',
      email: 'test@test.com',
    },
  } satisfies Required<UserSpec.EmailSenderConfig>,
} as const
