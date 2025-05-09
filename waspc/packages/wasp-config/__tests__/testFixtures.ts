/**
 * This module contains sample data that can be used for testing purposes.
 * In our case the sample data represents UserSpec data.
 */

import { GET_USER_SPEC } from '../src/_private.js'
import * as UserApi from '../src/userApi.js'

/**
 * This type removes all properties from T that are optional.
 */
type MinimalConfig<T> = {
  [K in keyof T as undefined extends T[K] ? never : K]: T[K]
}

/**
 * Required alias for domain consistency sake.
 */
type FullConfig<T> = Required<T>

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
  app.auth(AUTH.FULL)
  app.client(CLIENT.FULL)
  app.server(SERVER.FULL)
  app.emailSender(EMAIL_SENDER.FULL)
  app.webSocket(WEBSOCKET.FULL)
  app.db(DB.FULL)

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
    } satisfies FullConfig<UserApi.ExtImport>,
    DEFAULT: {
      from: '@src/external',
      importDefault: 'defaultExport',
    } satisfies FullConfig<UserApi.ExtImport>,
  },
  MINIMAL: {
    NAMED: {
      from: '@src/external',
      import: 'namedExport',
    } satisfies MinimalConfig<UserApi.ExtImport>,
    DEFAULT: {
      from: '@src/external',
      importDefault: 'defaultExport',
    } satisfies MinimalConfig<UserApi.ExtImport>,
  },
} as const

export const EMAIL_FROM_FIELD = {
  FULL: {
    name: 'ToDo App',
    email: 'test@domain.tld',
  } satisfies FullConfig<UserApi.EmailFromField>,
  MINIMAL: {
    email: 'test@domain.ltd',
  } satisfies MinimalConfig<UserApi.EmailFromField>,
} as const

export const PAGES = {
  MINIMAL: {
    NAME: 'MinimalPage',
    CONFIG: {
      component: EXT_IMPORT.MINIMAL.NAMED,
    } satisfies MinimalConfig<UserApi.PageConfig>,
  },
  FULL: {
    NAME: 'FullPage',
    CONFIG: {
      component: EXT_IMPORT.FULL.NAMED,
      authRequired: true,
    } satisfies FullConfig<UserApi.PageConfig>,
  },
  EMAIL_VERIFICATION: {
    NAME: 'EmailVerificationPage',
    CONFIG: {
      component: EXT_IMPORT.FULL.NAMED,
      authRequired: false,
    } satisfies FullConfig<UserApi.PageConfig>,
  },
  PASSWORD_RESET: {
    NAME: 'PasswordResetPage',
    CONFIG: {
      component: EXT_IMPORT.FULL.NAMED,
      authRequired: false,
    } satisfies FullConfig<UserApi.PageConfig>,
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
    } satisfies MinimalConfig<UserApi.RouteConfig>,
  },
  FULL: {
    NAME: 'FullRoute',
    CONFIG: {
      path: '/full',
      to: PAGES.FULL.NAME as string & { _brand: 'Page' },
    } satisfies FullConfig<UserApi.RouteConfig>,
  },
  EMAIL_VERIFICATION: {
    NAME: 'EmailVerificationRoute',
    CONFIG: {
      path: '/email-verification',
      to: PAGES.EMAIL_VERIFICATION.NAME as string & { _brand: 'Page' },
    } satisfies FullConfig<UserApi.RouteConfig>,
  },
  PASSWORD_RESET: {
    NAME: 'PasswordResetRoute',
    CONFIG: {
      path: '/password-reset',
      to: PAGES.PASSWORD_RESET.NAME as string & { _brand: 'Page' },
    } satisfies FullConfig<UserApi.RouteConfig>,
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
    } satisfies FullConfig<UserApi.QueryConfig>,
  },
  MINIMAL: {
    NAME: 'getTasks',
    CONFIG: {
      fn: EXT_IMPORT.MINIMAL.NAMED,
    } satisfies MinimalConfig<UserApi.QueryConfig>,
  },
} as const

export const ACTIONS = {
  FULL: {
    NAME: 'createTask',
    CONFIG: {
      fn: EXT_IMPORT.FULL.NAMED,
      entities: [TASK_ENTITY],
      auth: true,
    } satisfies FullConfig<UserApi.ActionConfig>,
  },
  MINIMAL: {
    NAME: 'deleteTask',
    CONFIG: {
      fn: EXT_IMPORT.MINIMAL.NAMED,
    } satisfies MinimalConfig<UserApi.ActionConfig>,
  },
} as const

export const CRUD_OPERATION_OPTIONS = {
  FULL: {
    isPublic: true,
    overrideFn: EXT_IMPORT.FULL.NAMED,
  } satisfies FullConfig<UserApi.CrudOperationOptions>,
  MINIMAL: {} satisfies MinimalConfig<UserApi.CrudOperationOptions>,
} as const

export const CRUD_OPERATIONS = {
  FULL: {
    get: CRUD_OPERATION_OPTIONS.FULL,
    getAll: CRUD_OPERATION_OPTIONS.FULL,
    create: CRUD_OPERATION_OPTIONS.FULL,
    update: CRUD_OPERATION_OPTIONS.FULL,
    delete: CRUD_OPERATION_OPTIONS.FULL,
  } satisfies FullConfig<UserApi.CrudOperations>,
  MINIMAL: {} satisfies MinimalConfig<UserApi.CrudOperations>,
} as const

export const CRUDS = {
  FULL: {
    NAME: 'TaskCrud',
    CONFIG: {
      entity: TASK_ENTITY,
      operations: CRUD_OPERATIONS.FULL,
    } satisfies FullConfig<UserApi.Crud>,
  },
  MINIMAL: {
    NAME: 'EmptyTaskCrud',
    CONFIG: {
      entity: TASK_ENTITY,
      operations: CRUD_OPERATIONS.MINIMAL,
    } satisfies MinimalConfig<UserApi.Crud>,
  },
} as const

export const API_NAMESPACES = {
  FULL: {
    NAME: 'bar',
    CONFIG: {
      middlewareConfigFn: EXT_IMPORT.FULL.NAMED,
      path: '/bar',
    } satisfies FullConfig<UserApi.ApiNamespaceConfig>,
  },
  MINIMAL: {
    NAME: 'foo',
    CONFIG: {
      middlewareConfigFn: EXT_IMPORT.MINIMAL.NAMED,
      path: '/foo',
    } satisfies MinimalConfig<UserApi.ApiNamespaceConfig>,
  },
} as const

export const HTTP_ROUTES = {
  FULL: {
    method: 'GET',
    route: '/bar/baz',
  } satisfies FullConfig<UserApi.HttpRoute>,
  MINIMAL: {
    method: 'POST',
    route: '/bar/foo',
  } satisfies MinimalConfig<UserApi.HttpRoute>,
} as const

export const APIS = {
  FULL: {
    NAME: 'barBaz',
    CONFIG: {
      fn: EXT_IMPORT.FULL.NAMED,
      auth: true,
      httpRoute: HTTP_ROUTES.FULL,
      entities: [TASK_ENTITY],
      middlewareConfigFn: EXT_IMPORT.FULL.NAMED,
    } satisfies FullConfig<UserApi.ApiConfig>,
  },
  MINIMAL: {
    NAME: 'barFoo',
    CONFIG: {
      fn: EXT_IMPORT.MINIMAL.NAMED,
      httpRoute: HTTP_ROUTES.MINIMAL,
    } satisfies MinimalConfig<UserApi.ApiConfig>,
  },
} as const

export const SCHEDULE = {
  FULL: {
    cron: '0 0 * * *',
    args: { foo: 'bar' },
    executorOptions: {
      pgBoss: { jobOptions: { attempts: 3 } },
    },
  } satisfies FullConfig<UserApi.ScheduleConfig>,
  MINIMAL: {
    cron: '0 0 * * *',
  } satisfies MinimalConfig<UserApi.ScheduleConfig>,
}

export const PERFORM = {
  FULL: {
    fn: EXT_IMPORT.FULL.NAMED,
    executorOptions: {
      pgBoss: { jobOptions: { attempts: 3 } },
    },
  } satisfies FullConfig<UserApi.Perform>,
  MINIMAL: {
    fn: EXT_IMPORT.MINIMAL.NAMED,
  } satisfies MinimalConfig<UserApi.Perform>,
}

export const JOBS = {
  FULL: {
    NAME: 'mySpecialJob',
    CONFIG: {
      executor: 'PgBoss',
      entities: [TASK_ENTITY],
      perform: PERFORM.FULL,
      schedule: SCHEDULE.FULL,
    } satisfies FullConfig<UserApi.JobConfig>,
  },
  MINIMAL: {
    NAME: 'mySimpleJob',
    CONFIG: {
      executor: 'PgBoss',
      perform: PERFORM.MINIMAL,
    } satisfies MinimalConfig<UserApi.JobConfig>,
  },
} as const

export const APP = {
  FULL: {
    NAME: 'todoApp',
    CONFIG: {
      title: 'Todo App',
      wasp: { version: '0.16.3' },
      head: ['<link rel="icon" href="/favicon.ico" />'],
    } satisfies FullConfig<UserApi.AppConfig>,
  },
  MINIMAL: {
    NAME: 'minimalApp',
    CONFIG: {
      title: 'Minimal App',
      wasp: { version: '0.16.3' },
    } satisfies MinimalConfig<UserApi.AppConfig>,
  },
} as const

export const EMAIL_VERIFICATION = {
  FULL: {
    getEmailContentFn: EXT_IMPORT.FULL.NAMED,
    clientRoute: ROUTES.EMAIL_VERIFICATION.NAME,
  } satisfies FullConfig<UserApi.EmailVerificationConfig>,
  MINIMAL: {
    clientRoute: ROUTES.EMAIL_VERIFICATION.NAME,
  } satisfies MinimalConfig<UserApi.EmailVerificationConfig>,
} as const

export const PASSWORD_RESET = {
  FULL: {
    getEmailContentFn: EXT_IMPORT.FULL.NAMED,
    clientRoute: ROUTES.PASSWORD_RESET.NAME,
  } satisfies FullConfig<UserApi.PasswordResetConfig>,
  MINIMAL: {
    clientRoute: ROUTES.PASSWORD_RESET.NAME,
  } satisfies MinimalConfig<UserApi.PasswordResetConfig>,
} as const

export const EMAIL_AUTH = {
  FULL: {
    userSignupFields: EXT_IMPORT.FULL.NAMED,
    fromField: EMAIL_FROM_FIELD.FULL,
    emailVerification: EMAIL_VERIFICATION.FULL,
    passwordReset: PASSWORD_RESET.FULL,
  } satisfies FullConfig<UserApi.EmailAuthConfig>,
  MINIMAL: {
    fromField: EMAIL_FROM_FIELD.MINIMAL,
    emailVerification: EMAIL_VERIFICATION.MINIMAL,
    passwordReset: PASSWORD_RESET.MINIMAL,
  } satisfies MinimalConfig<UserApi.EmailAuthConfig>,
} as const

export const USERNAME_AND_PASSWORD_AUTH = {
  FULL: {
    userSignupFields: EXT_IMPORT.FULL.NAMED,
  } satisfies FullConfig<UserApi.UsernameAndPasswordConfig>,
  MINIMAL: {} satisfies MinimalConfig<UserApi.UsernameAndPasswordConfig>,
} as const

export const EXTERNAL_AUTH = {
  FULL: {
    configFn: EXT_IMPORT.FULL.NAMED,
    userSignupFields: EXT_IMPORT.FULL.NAMED,
  } satisfies FullConfig<UserApi.ExternalAuthConfig>,
  MINIMAL: {} satisfies MinimalConfig<UserApi.ExternalAuthConfig>,
} as const

export const AUTH_METHODS = {
  FULL: {
    email: EMAIL_AUTH.FULL,
    discord: EXTERNAL_AUTH.FULL,
    google: EXTERNAL_AUTH.FULL,
    gitHub: EXTERNAL_AUTH.FULL,
    keycloak: EXTERNAL_AUTH.FULL,
    usernameAndPassword: USERNAME_AND_PASSWORD_AUTH.FULL,
  } satisfies FullConfig<UserApi.AuthMethods>,
  MINIMAL: {} satisfies MinimalConfig<UserApi.AuthMethods>,
} as const

export const AUTH = {
  FULL: {
    userEntity: USER_ENTITY,
    externalAuthEntity: SOCIAL_USER_ENTITY,
    onAuthFailedRedirectTo: '/login',
    onAuthSucceededRedirectTo: '/profile',
    methods: AUTH_METHODS.FULL,
    onBeforeSignup: EXT_IMPORT.FULL.NAMED,
    onAfterSignup: EXT_IMPORT.FULL.NAMED,
    onBeforeOAuthRedirect: EXT_IMPORT.FULL.NAMED,
    onBeforeLogin: EXT_IMPORT.FULL.NAMED,
    onAfterLogin: EXT_IMPORT.FULL.NAMED,
  } satisfies FullConfig<UserApi.AuthConfig>,
  MINIMAL: {
    userEntity: USER_ENTITY,
    onAuthFailedRedirectTo: '/login',
    methods: AUTH_METHODS.MINIMAL,
  } satisfies MinimalConfig<UserApi.AuthConfig>,
} as const

export const CLIENT = {
  FULL: {
    rootComponent: EXT_IMPORT.FULL.NAMED,
    setupFn: EXT_IMPORT.FULL.NAMED,
    baseDir: '/src',
    envValidationSchema: EXT_IMPORT.FULL.NAMED,
  } satisfies FullConfig<UserApi.ClientConfig>,
  MINIMAL: {} satisfies MinimalConfig<UserApi.ClientConfig>,
} as const

export const SERVER = {
  FULL: {
    setupFn: EXT_IMPORT.FULL.NAMED,
    middlewareConfigFn: EXT_IMPORT.FULL.NAMED,
    envValidationSchema: EXT_IMPORT.FULL.NAMED,
  } satisfies FullConfig<UserApi.ServerConfig>,
  MINIMAL: {} satisfies MinimalConfig<UserApi.ServerConfig>,
} as const

export const EMAIL_SENDER = {
  FULL: {
    provider: 'SMTP',
    defaultFrom: EMAIL_FROM_FIELD.FULL,
  } satisfies FullConfig<UserApi.EmailSenderConfig>,
  MINIMAL: {
    provider: 'SMTP',
  } satisfies MinimalConfig<UserApi.EmailSenderConfig>,
} as const

export const WEBSOCKET = {
  FULL: {
    fn: EXT_IMPORT.FULL.NAMED,
    autoConnect: true,
  } satisfies FullConfig<UserApi.WebsocketConfig>,
  MINIMAL: {
    fn: EXT_IMPORT.MINIMAL.NAMED,
  } satisfies MinimalConfig<UserApi.WebsocketConfig>,
} as const

export const DB = {
  FULL: {
    seeds: [EXT_IMPORT.FULL.NAMED, EXT_IMPORT.FULL.DEFAULT],
  } satisfies FullConfig<UserApi.DbConfig>,
  MINIMAL: {} satisfies MinimalConfig<UserApi.DbConfig>,
} as const
