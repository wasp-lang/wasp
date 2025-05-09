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
  app.auth(AUTH.FULL.CONFIG)
  app.client(CLIENT.FULL.CONFIG)
  app.server(SERVER.FULL.CONFIG)
  app.emailSender(EMAIL_SENDER.FULL.CONFIG)
  app.webSocket(WEBSOCKET.FULL.CONFIG)
  app.db(DB.FULL.CONFIG)

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

export const EXT_IMPORT = {
  FULL: {
    NAMED: {
      from: '@src/external',
      import: 'namedExport',
    } satisfies UserApi.ExtImport,
    DEFAULT: {
      from: '@src/external',
      importDefault: 'defaultExport',
    } satisfies UserApi.ExtImport,
  },
  MINIMAL: {
    NAMED: {
      from: '@src/external',
      import: 'namedExport',
    } satisfies UserApi.ExtImport,
    DEFAULT: {
      from: '@src/external',
      importDefault: 'defaultExport',
    } satisfies UserApi.ExtImport,
  },
} as const

export const EMAIL_FROM_FIELD = {
  FULL: {
    CONFIG: {
      name: 'ToDo App',
      email: 'test@domain.tld',
    } satisfies Required<UserApi.EmailFromField>,
  },
  MINIMAL: {
    CONFIG: {
      email: 'test@domain.ltd',
    } satisfies UserApi.EmailFromField,
  },
} as const

export const PAGES = {
  MINIMAL: {
    NAME: 'MinimalPage',
    CONFIG: {
      component: EXT_IMPORT.MINIMAL.NAMED,
    } satisfies UserApi.PageConfig,
  },
  FULL: {
    NAME: 'FullPage',
    CONFIG: {
      component: EXT_IMPORT.FULL.NAMED,
      authRequired: true,
    } satisfies Required<UserApi.PageConfig>,
  },
  EMAIL_VERIFICATION: {
    NAME: 'EmailVerificationPage',
    CONFIG: {
      component: EXT_IMPORT.FULL.NAMED,
      authRequired: false,
    } satisfies Required<UserApi.PageConfig>,
  },
  PASSWORD_RESET: {
    NAME: 'PasswordResetPage',
    CONFIG: {
      component: EXT_IMPORT.FULL.NAMED,
      authRequired: false,
    } satisfies Required<UserApi.PageConfig>,
  },
} as const
export const ALL_PAGE_NAMES = Object.values(PAGES).map((page) => page.NAME)

// For simplicity sake we asserted `RouteConfig.to` as branded type
// instead of creating a function which would accept branded string.
export const ROUTES = {
  MINIMAL: {
    NAME: 'MinimalRoute',
    CONFIG: {
      path: '/minimal',
      to: PAGES.MINIMAL.NAME as string & { _brand: 'Page' },
    } satisfies UserApi.RouteConfig,
  },
  FULL: {
    NAME: 'FullRoute',
    CONFIG: {
      path: '/full',
      to: PAGES.FULL.NAME as string & { _brand: 'Page' },
    } satisfies Required<UserApi.RouteConfig>,
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
      fn: EXT_IMPORT.FULL.NAMED,
      entities: [TASK_ENTITY],
      auth: true,
    } satisfies Required<UserApi.QueryConfig>,
  },
  MINIMAL: {
    NAME: 'getTasks',
    CONFIG: {
      fn: EXT_IMPORT.MINIMAL.NAMED,
    } satisfies UserApi.QueryConfig,
  },
} as const

export const ACTIONS = {
  FULL: {
    NAME: 'createTask',
    CONFIG: {
      fn: EXT_IMPORT.FULL.NAMED,
      entities: [TASK_ENTITY],
      auth: true,
    } satisfies Required<UserApi.ActionConfig>,
  },
  MINIMAL: {
    NAME: 'deleteTask',
    CONFIG: {
      fn: EXT_IMPORT.MINIMAL.NAMED,
    } satisfies UserApi.ActionConfig,
  },
} as const

export const CRUD_OPERATION_OPTIONS = {
  FULL: {
    CONFIG: {
      isPublic: true,
      overrideFn: EXT_IMPORT.FULL.NAMED,
    } satisfies Required<UserApi.CrudOperationOptions>,
  },
  MINIMAL: {
    CONFIG: {} satisfies UserApi.CrudOperationOptions,
  },
} as const

export const CRUD_OPERATIONS = {
  FULL: {
    CONFIG: {
      get: CRUD_OPERATION_OPTIONS.FULL.CONFIG,
      getAll: CRUD_OPERATION_OPTIONS.FULL.CONFIG,
      create: CRUD_OPERATION_OPTIONS.FULL.CONFIG,
      update: CRUD_OPERATION_OPTIONS.FULL.CONFIG,
      delete: CRUD_OPERATION_OPTIONS.FULL.CONFIG,
    } satisfies Required<UserApi.CrudOperations>,
  },
  MINIMAL: {
    CONFIG: {} satisfies UserApi.CrudOperations,
  },
} as const

export const CRUDS = {
  FULL: {
    NAME: 'TaskCrud',
    CONFIG: {
      entity: TASK_ENTITY,
      operations: CRUD_OPERATIONS.FULL.CONFIG,
    } satisfies Required<UserApi.Crud>,
  },
  MINIMAL: {
    NAME: 'EmptyTaskCrud',
    CONFIG: {
      entity: TASK_ENTITY,
      operations: CRUD_OPERATIONS.MINIMAL.CONFIG,
    } satisfies UserApi.Crud,
  },
} as const

export const API_NAMESPACES = {
  FULL: {
    NAME: 'bar',
    CONFIG: {
      middlewareConfigFn: EXT_IMPORT.FULL.NAMED,
      path: '/bar',
    } satisfies Required<UserApi.ApiNamespaceConfig>,
  },
  MINIMAL: {
    NAME: 'foo',
    CONFIG: {
      middlewareConfigFn: EXT_IMPORT.MINIMAL.NAMED,
      path: '/foo',
    } satisfies UserApi.ApiNamespaceConfig,
  },
} as const

export const HTTP_ROUTES = {
  FULL: {
    CONFIG: {
      method: 'GET',
      route: '/bar/baz',
    } satisfies Required<UserApi.HttpRoute>,
  },
  MINIMAL: {
    CONFIG: {
      method: 'POST',
      route: '/bar/foo',
    } satisfies UserApi.HttpRoute,
  },
} as const

export const APIS = {
  FULL: {
    NAME: 'barBaz',
    CONFIG: {
      fn: EXT_IMPORT.FULL.NAMED,
      auth: true,
      httpRoute: HTTP_ROUTES.FULL.CONFIG,
      entities: [TASK_ENTITY],
      middlewareConfigFn: EXT_IMPORT.FULL.NAMED,
    } satisfies Required<UserApi.ApiConfig>,
  },
  MINIMAL: {
    NAME: 'barFoo',
    CONFIG: {
      fn: EXT_IMPORT.MINIMAL.NAMED,
      httpRoute: HTTP_ROUTES.MINIMAL.CONFIG,
    } satisfies UserApi.ApiConfig,
  },
} as const

export const SCHEDULE = {
  FULL: {
    CONFIG: {
      cron: '0 0 * * *',
      args: { foo: 'bar' },
      executorOptions: {
        pgBoss: { jobOptions: { attempts: 3 } },
      },
    } satisfies Required<UserApi.ScheduleConfig>,
  },
  MINIMAL: {
    CONFIG: {
      cron: '0 0 * * *',
    } satisfies UserApi.ScheduleConfig,
  },
}

export const PERFORM = {
  FULL: {
    CONFIG: {
      fn: EXT_IMPORT.FULL.NAMED,
      executorOptions: {
        pgBoss: { jobOptions: { attempts: 3 } },
      },
    } satisfies Required<UserApi.Perform>,
  },
  MINIMAL: {
    CONFIG: {
      fn: EXT_IMPORT.MINIMAL.NAMED,
    } satisfies UserApi.Perform,
  },
}

export const JOBS = {
  FULL: {
    NAME: 'mySpecialJob',
    CONFIG: {
      executor: 'PgBoss',
      entities: [TASK_ENTITY],
      perform: PERFORM.FULL.CONFIG,
      schedule: SCHEDULE.FULL.CONFIG,
    } satisfies Required<UserApi.JobConfig>,
  },
  MINIMAL: {
    NAME: 'mySimpleJob',
    CONFIG: {
      executor: 'PgBoss',
      perform: PERFORM.MINIMAL.CONFIG,
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

export const EMAIL_VERIFICATION = {
  FULL: {
    CONFIG: {
      getEmailContentFn: EXT_IMPORT.FULL.NAMED,
      clientRoute: ROUTES.EMAIL_VERIFICATION.NAME,
    } satisfies Required<UserApi.EmailVerificationConfig>,
  },
  MINIMAL: {
    CONFIG: {
      clientRoute: ROUTES.EMAIL_VERIFICATION.NAME,
    } satisfies UserApi.EmailVerificationConfig,
  },
} as const

export const PASSWORD_RESET = {
  FULL: {
    CONFIG: {
      getEmailContentFn: EXT_IMPORT.FULL.NAMED,
      clientRoute: ROUTES.PASSWORD_RESET.NAME,
    } satisfies Required<UserApi.PasswordResetConfig>,
  },
  MINIMAL: {
    CONFIG: {
      clientRoute: ROUTES.PASSWORD_RESET.NAME,
    } satisfies UserApi.PasswordResetConfig,
  },
} as const

export const EMAIL_AUTH = {
  FULL: {
    CONFIG: {
      userSignupFields: EXT_IMPORT.FULL.NAMED,
      fromField: EMAIL_FROM_FIELD.FULL.CONFIG,
      emailVerification: EMAIL_VERIFICATION.FULL.CONFIG,
      passwordReset: PASSWORD_RESET.FULL.CONFIG,
    } satisfies Required<UserApi.EmailAuthConfig>,
  },
  MINIMAL: {
    CONFIG: {
      fromField: EMAIL_FROM_FIELD.MINIMAL.CONFIG,
      emailVerification: EMAIL_VERIFICATION.MINIMAL.CONFIG,
      passwordReset: PASSWORD_RESET.MINIMAL.CONFIG,
    } satisfies UserApi.EmailAuthConfig,
  },
} as const

export const USERNAME_AND_PASSWORD_AUTH = {
  FULL: {
    CONFIG: {
      userSignupFields: EXT_IMPORT.FULL.NAMED,
    } satisfies Required<UserApi.UsernameAndPasswordConfig>,
  },
  MINIMAL: {
    CONFIG: {} satisfies UserApi.UsernameAndPasswordConfig,
  },
} as const

export const EXTERNAL_AUTH = {
  FULL: {
    CONFIG: {
      configFn: EXT_IMPORT.FULL.NAMED,
      userSignupFields: EXT_IMPORT.FULL.NAMED,
    } satisfies Required<UserApi.ExternalAuthConfig>,
  },
  MINIMAL: {
    CONFIG: {} satisfies UserApi.ExternalAuthConfig,
  },
} as const

export const AUTH_METHODS = {
  FULL: {
    CONFIG: {
      email: EMAIL_AUTH.FULL.CONFIG,
      discord: EXTERNAL_AUTH.FULL.CONFIG,
      google: EXTERNAL_AUTH.FULL.CONFIG,
      gitHub: EXTERNAL_AUTH.FULL.CONFIG,
      keycloak: EXTERNAL_AUTH.FULL.CONFIG,
      usernameAndPassword: USERNAME_AND_PASSWORD_AUTH.FULL.CONFIG,
    } satisfies Required<UserApi.AuthMethods>,
  },
  MINIMAL: {
    CONFIG: {} satisfies UserApi.AuthMethods,
  },
} as const

export const AUTH = {
  FULL: {
    CONFIG: {
      userEntity: USER_ENTITY,
      externalAuthEntity: SOCIAL_USER_ENTITY,
      onAuthFailedRedirectTo: '/login',
      onAuthSucceededRedirectTo: '/profile',
      methods: AUTH_METHODS.FULL.CONFIG,
      onBeforeSignup: EXT_IMPORT.FULL.NAMED,
      onAfterSignup: EXT_IMPORT.FULL.NAMED,
      onBeforeOAuthRedirect: EXT_IMPORT.FULL.NAMED,
      onBeforeLogin: EXT_IMPORT.FULL.NAMED,
      onAfterLogin: EXT_IMPORT.FULL.NAMED,
    } satisfies Required<UserApi.AuthConfig>,
  },
  MINIMAL: {
    CONFIG: {
      userEntity: USER_ENTITY,
      onAuthFailedRedirectTo: '/login',
      methods: AUTH_METHODS.MINIMAL.CONFIG,
    } satisfies UserApi.AuthConfig,
  },
} as const

export const CLIENT = {
  FULL: {
    CONFIG: {
      rootComponent: EXT_IMPORT.FULL.NAMED,
      setupFn: EXT_IMPORT.FULL.NAMED,
      baseDir: '/src',
      envValidationSchema: EXT_IMPORT.FULL.NAMED,
    } satisfies Required<UserApi.ClientConfig>,
  },
  MINIMAL: {
    CONFIG: {} satisfies UserApi.ClientConfig,
  },
} as const

export const SERVER = {
  FULL: {
    CONFIG: {
      setupFn: EXT_IMPORT.FULL.NAMED,
      middlewareConfigFn: EXT_IMPORT.FULL.NAMED,
      envValidationSchema: EXT_IMPORT.FULL.NAMED,
    } satisfies Required<UserApi.ServerConfig>,
  },
  MINIMAL: {
    CONFIG: {} satisfies UserApi.ServerConfig,
  },
} as const

export const EMAIL_SENDER = {
  FULL: {
    CONFIG: {
      provider: 'SMTP',
      defaultFrom: EMAIL_FROM_FIELD.FULL.CONFIG,
    } satisfies Required<UserApi.EmailSenderConfig>,
  },
  MINIMAL: {
    CONFIG: {
      provider: 'SMTP',
    } satisfies UserApi.EmailSenderConfig,
  },
} as const

export const WEBSOCKET = {
  FULL: {
    CONFIG: {
      fn: EXT_IMPORT.FULL.NAMED,
      autoConnect: true,
    } satisfies Required<UserApi.WebsocketConfig>,
  },
  MINIMAL: {
    CONFIG: {
      fn: EXT_IMPORT.MINIMAL.NAMED,
    } satisfies UserApi.WebsocketConfig,
  },
} as const

export const DB = {
  FULL: {
    CONFIG: {
      seeds: [EXT_IMPORT.FULL.NAMED, EXT_IMPORT.FULL.DEFAULT],
    } satisfies Required<UserApi.DbConfig>,
  },
  MINIMAL: {
    CONFIG: {} satisfies UserApi.DbConfig,
  },
} as const
