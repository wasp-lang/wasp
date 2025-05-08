import { GET_USER_SPEC } from '../src/_private.js'
import * as UserApi from '../src/userApi.js'

// Contains sample data that can be used for testing purposes.
// In our case the sample data represents UserSpec data.

type ConfigType = 'minimal' | 'full'

export function createUserApp(configType: ConfigType): UserApi.App {
  if (configType === 'minimal') {
    return createMinimalUserApp()
  } else if (configType === 'full') {
    return createFullUserApp()
  } else {
    throw new Error(`Unknown config type: ${configType}`)
  }
}

export function createUserSpec(configType: ConfigType): UserApi.UserSpec {
  if (configType === 'minimal') {
    return createMinimalUserApp()[GET_USER_SPEC]()
  } else if (configType === 'full') {
    return createFullUserApp()[GET_USER_SPEC]()
  } else {
    throw new Error(`Unknown config type: ${configType}`)
  }
}

function createMinimalUserApp(): UserApi.App {
  return new UserApi.App(APP.MINIMAL.NAME, APP.MINIMAL.CONFIG)
}

function createFullUserApp(): UserApi.App {
  const app = new UserApi.App(APP.FULL.NAME, APP.FULL.CONFIG)
  app.auth(AUTH.CONFIG)
  app.client(CLIENT.CONFIG)
  app.server(SERVER.CONFIG)
  app.emailSender(EMAIL_SENDER.CONFIG)
  app.webSocket(WEBSOCKET.CONFIG)
  app.db(DB.CONFIG)

  function addDecls(
    declName: string,
    nameAndConfigs: Record<string, { NAME: string; CONFIG: unknown }>
  ) {
    Object.values(nameAndConfigs).forEach(({ NAME, CONFIG }) =>
      app[declName](NAME, CONFIG)
    )
  }

  addDecls('page', PAGES)
  addDecls('route', ROUTES)
  addDecls('query', QUERIES)
  addDecls('action', ACTIONS)
  addDecls('crud', CRUDS)
  addDecls('apiNamespace', API_NAMESPACES)
  addDecls('api', APIS)
  addDecls('job', JOBS)

  return app
}

export const TASK_ENTITY = 'Task'
export const USER_ENTITY = 'User'
export const SOCIAL_USER_ENTITY = 'SocialUser'
export const ALL_ENTITIES = [TASK_ENTITY, USER_ENTITY, SOCIAL_USER_ENTITY]

export const PAGES = {
  MINIMAL: {
    NAME: 'MinimalPage',
    CONFIG: {
      component: {
        from: '@src/pages/minimal',
        import: 'Minimal',
      },
    } satisfies UserApi.PageConfig,
  },
  FULL: {
    NAME: 'FullPage',
    CONFIG: {
      component: {
        from: '@src/pages/full',
        import: 'Full',
      },
      authRequired: true,
    } satisfies UserApi.PageConfig,
  },
  EMAIL_VERIFICATION: {
    NAME: 'EmailVerificationPage',
    CONFIG: {
      component: {
        from: '@src/pages/auth/EmailVerification',
        import: 'EmailVerification',
      },
      authRequired: false,
    } satisfies Required<UserApi.PageConfig>,
  },
  PASSWORD_RESET: {
    NAME: 'PasswordResetPage',
    CONFIG: {
      component: {
        from: '@src/pages/auth/PasswordReset',
        import: 'PasswordReset',
      },
      authRequired: false,
    } satisfies Required<UserApi.PageConfig>,
  },
} as const
export const ALL_PAGE_NAMES = Object.values(PAGES).map((page) => page.NAME)

// For simplicity sake we asserted `RouteConfig.to` as branded type
// instead of creating a function which would accept branded string.
export const ROUTES = {
  FULL: {
    NAME: 'FullRoute',
    CONFIG: {
      path: '/full',
      to: PAGES.FULL.NAME as string & { _brand: 'Page' },
    } satisfies UserApi.RouteConfig,
  },
  EMAIL_VERIFICATION: {
    NAME: 'EmailVerificationRoute',
    CONFIG: {
      path: '/email-verification',
      to: PAGES.EMAIL_VERIFICATION.NAME as string & { _brand: 'Page' },
    } satisfies Required<UserApi.RouteConfig>,
  },
  PASSWORD_RESET: {
    NAME: 'PasswordResetRoute',
    CONFIG: {
      path: '/password-reset',
      to: PAGES.PASSWORD_RESET.NAME as string & { _brand: 'Page' },
    } satisfies Required<UserApi.RouteConfig>,
  },
} as const
export const ALL_ROUTE_NAMES = Object.values(ROUTES).map((route) => route.NAME)

export const QUERIES = {
  FULL: {
    NAME: 'getTask',
    CONFIG: {
      fn: {
        import: 'getTask',
        from: '@src/queries',
      },
      entities: [TASK_ENTITY],
      auth: true,
    } satisfies Required<UserApi.QueryConfig>,
  },
  MINIMAL: {
    NAME: 'getTasks',
    CONFIG: {
      fn: {
        import: 'getTasks',
        from: '@src/queries',
      },
    } satisfies UserApi.QueryConfig,
  },
} as const

export const ACTIONS = {
  FULL: {
    NAME: 'createTask',
    CONFIG: {
      fn: {
        import: 'createTask',
        from: '@src/actions',
      },
      entities: [TASK_ENTITY],
      auth: true,
    } satisfies Required<UserApi.ActionConfig>,
  },
  MINIMAL: {
    NAME: 'deleteTask',
    CONFIG: {
      fn: {
        import: 'deleteTask',
        from: '@src/actions',
      },
    } satisfies UserApi.ActionConfig,
  },
} as const

export const CRUDS = {
  FULL: {
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
          overrideFn: {
            import: 'getAllTasks',
            from: '@src/cruds/task',
          },
        },
        create: {
          isPublic: false,
          overrideFn: {
            import: 'createTask',
            from: '@src/cruds/task',
          },
        },
        update: {
          isPublic: false,
          overrideFn: {
            import: 'updateTask',
            from: '@src/cruds/task',
          },
        },
        delete: {
          isPublic: false,
          overrideFn: {
            import: 'deleteTask',
            from: '@src/cruds/task',
          },
        },
      },
    } satisfies Required<UserApi.Crud>,
  },
  MINIMAL: {
    NAME: 'EmptyTaskCrud',
    CONFIG: {
      entity: TASK_ENTITY,
      operations: {},
    } satisfies UserApi.Crud,
  },
} as const

export const API_NAMESPACES = {
  FULL: {
    NAME: 'bar',
    CONFIG: {
      middlewareConfigFn: {
        import: 'barMiddlewareConfigFn',
        from: '@src/apis',
      },
      path: '/bar',
    } satisfies Required<UserApi.ApiNamespaceConfig>,
  },
} as const

export const APIS = {
  FULL: {
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
    } satisfies Required<UserApi.ApiConfig>,
  },
  MINIMAL: {
    NAME: 'barFoo',
    CONFIG: {
      fn: {
        import: 'barFoo',
        from: '@src/apis',
      },
      httpRoute: {
        method: 'POST',
        route: '/bar/foo',
      },
    } satisfies UserApi.ApiConfig,
  },
} as const

export const JOBS = {
  FULL: {
    NAME: 'mySpecialJob',
    CONFIG: {
      executor: 'PgBoss',
      perform: {
        fn: {
          import: 'perform',
          from: '@src/jobs/bar',
        },
        executorOptions: {
          pgBoss: { jobOptions: { attempts: 3 } },
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
    } satisfies Required<UserApi.JobConfig>,
  },
  MINIMAL: {
    NAME: 'mySimpleJob',
    CONFIG: {
      executor: 'PgBoss',
      perform: {
        fn: {
          import: 'perform',
          from: '@src/jobs/bar',
        },
      },
    } satisfies UserApi.JobConfig,
  },
} as const

export const APP = {
  FULL: {
    NAME: 'todoApp',
    CONFIG: {
      title: 'Todo App',
      wasp: { version: '0.16.3' },
      head: ['<link rel="icon" href="/favicon.ico" />'],
    } satisfies Required<UserApi.AppConfig>,
  },
  MINIMAL: {
    NAME: 'minimalApp',
    CONFIG: {
      title: 'Minimal App',
      wasp: { version: '0.16.3' },
    } satisfies UserApi.AppConfig,
  },
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
          clientRoute: ROUTES.EMAIL_VERIFICATION.NAME,
        },
        passwordReset: {
          getEmailContentFn: {
            import: 'getPasswordResetEmailContent',
            from: '@src/auth/email',
          },
          clientRoute: ROUTES.PASSWORD_RESET.NAME,
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
  } satisfies Required<UserApi.AuthConfig>,
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
  } satisfies Required<UserApi.ClientConfig>,
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
  } satisfies Required<UserApi.ServerConfig>,
} as const

export const EMAIL_SENDER = {
  CONFIG: {
    provider: 'SMTP',
    defaultFrom: {
      name: 'Test',
      email: 'test@test.com',
    },
  } satisfies Required<UserApi.EmailSenderConfig>,
} as const

export const WEBSOCKET = {
  CONFIG: {
    fn: {
      import: 'webSocketFn',
      from: '@src/webSocket',
    },
    autoConnect: true,
  } satisfies Required<UserApi.WebsocketConfig>,
} as const

export const DB = {
  CONFIG: {
    seeds: [
      {
        import: 'devSeedSimple',
        from: '@src/dbSeeds',
      },
    ],
  } satisfies Required<UserApi.DbConfig>,
} as const
