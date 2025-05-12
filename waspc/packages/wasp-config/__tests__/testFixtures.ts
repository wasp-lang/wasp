/**
 * This module contains sample data that can be used for testing purposes.
 * In our case the sample data represents UserSpec data.
 */

import * as AppSpec from '../src/appSpec.js'
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

function createMinimalUserApp(): UserApi.App {
  return new UserApi.App(APP.MINIMAL.name, APP.MINIMAL.config)
}

function createFullUserApp(): UserApi.App {
  const app = new UserApi.App(APP.FULL.name, APP.FULL.config)
  app.auth(AUTH.FULL)
  app.client(CLIENT.FULL)
  app.server(SERVER.FULL)
  app.emailSender(EMAIL_SENDER.FULL)
  app.webSocket(WEBSOCKET.FULL)
  app.db(DB.FULL)

  function addDecls(
    declName: string,
    nameAndConfigs: Record<string, { name: string; config: unknown }>
  ) {
    Object.values(nameAndConfigs).forEach(({ name, config }) =>
      app[declName](name, config)
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

const TASK_ENTITY = 'Task'
const USER_ENTITY = 'User'
const SOCIAL_USER_ENTITY = 'SocialUser'

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
    name: 'MinimalPage',
    config: {
      component: getExtImport('minimal'),
    } satisfies MinimalConfig<UserApi.PageConfig>,
  },
  FULL: {
    name: 'FullPage',
    config: {
      component: getExtImport('full'),
      authRequired: true,
    } satisfies FullConfig<UserApi.PageConfig>,
  },
  EMAIL_VERIFICATION: {
    name: 'EmailVerificationPage',
    config: {
      component: getExtImport('full'),
      authRequired: false,
    } satisfies FullConfig<UserApi.PageConfig>,
  },
  PASSWORD_RESET: {
    name: 'PasswordResetPage',
    config: {
      component: getExtImport('full'),
      authRequired: false,
    } satisfies FullConfig<UserApi.PageConfig>,
  },
} as const
export const ALL_PAGE_NAMES = Object.values(PAGES).map((page) => page.name)

// For simplicity sake we asserted `RouteConfig.to` as branded type
// instead of creating a function which would accept branded string.
export const ROUTES = {
  MINIMAL: {
    name: 'MinimalRoute',
    config: {
      path: '/minimal',
      to: PAGES.MINIMAL.name as string & { _brand: 'Page' },
    } satisfies MinimalConfig<UserApi.RouteConfig>,
  },
  FULL: {
    name: 'FullRoute',
    config: {
      path: '/full',
      to: PAGES.FULL.name as string & { _brand: 'Page' },
    } satisfies FullConfig<UserApi.RouteConfig>,
  },
  EMAIL_VERIFICATION: {
    name: 'EmailVerificationRoute',
    config: {
      path: '/email-verification',
      to: PAGES.EMAIL_VERIFICATION.name as string & { _brand: 'Page' },
    } satisfies FullConfig<UserApi.RouteConfig>,
  },
  PASSWORD_RESET: {
    name: 'PasswordResetRoute',
    config: {
      path: '/password-reset',
      to: PAGES.PASSWORD_RESET.name as string & { _brand: 'Page' },
    } satisfies FullConfig<UserApi.RouteConfig>,
  },
} as const
export const ALL_ROUTE_NAMES = Object.values(ROUTES).map((route) => route.name)

export const QUERIES = {
  FULL: {
    name: 'getTask',
    config: {
      fn: getExtImport('full'),
      entities: [TASK_ENTITY],
      auth: true,
    } satisfies FullConfig<UserApi.QueryConfig>,
  },
  MINIMAL: {
    name: 'getTasks',
    config: {
      fn: getExtImport('minimal'),
    } satisfies MinimalConfig<UserApi.QueryConfig>,
  },
} as const

export const ACTIONS = {
  FULL: {
    name: 'createTask',
    config: {
      fn: getExtImport('full'),
      entities: [TASK_ENTITY],
      auth: true,
    } satisfies FullConfig<UserApi.ActionConfig>,
  },
  MINIMAL: {
    name: 'deleteTask',
    config: {
      fn: getExtImport('minimal'),
    } satisfies MinimalConfig<UserApi.ActionConfig>,
  },
} as const

export const CRUD_OPERATION_OPTIONS = {
  FULL: {
    isPublic: true,
    overrideFn: getExtImport('full'),
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
    name: 'TaskCrud',
    config: {
      entity: TASK_ENTITY,
      operations: CRUD_OPERATIONS.FULL,
    } satisfies FullConfig<UserApi.Crud>,
  },
  MINIMAL: {
    name: 'EmptyTaskCrud',
    config: {
      entity: TASK_ENTITY,
      operations: CRUD_OPERATIONS.MINIMAL,
    } satisfies MinimalConfig<UserApi.Crud>,
  },
} as const

export const API_NAMESPACES = {
  FULL: {
    name: 'bar',
    config: {
      middlewareConfigFn: getExtImport('full'),
      path: '/bar',
    } satisfies FullConfig<UserApi.ApiNamespaceConfig>,
  },
  MINIMAL: {
    name: 'foo',
    config: {
      middlewareConfigFn: getExtImport('minimal'),
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
    name: 'barBaz',
    config: {
      fn: getExtImport('full'),
      auth: true,
      httpRoute: HTTP_ROUTES.FULL,
      entities: [TASK_ENTITY],
      middlewareConfigFn: getExtImport('full'),
    } satisfies FullConfig<UserApi.ApiConfig>,
  },
  MINIMAL: {
    name: 'barFoo',
    config: {
      fn: getExtImport('minimal'),
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
    fn: getExtImport('full'),
    executorOptions: {
      pgBoss: { jobOptions: { attempts: 3 } },
    },
  } satisfies FullConfig<UserApi.Perform>,
  MINIMAL: {
    fn: getExtImport('minimal'),
  } satisfies MinimalConfig<UserApi.Perform>,
}

export const JOBS = {
  FULL: {
    name: 'mySpecialJob',
    config: {
      executor: 'PgBoss',
      entities: [TASK_ENTITY],
      perform: PERFORM.FULL,
      schedule: SCHEDULE.FULL,
    } satisfies FullConfig<UserApi.JobConfig>,
  },
  MINIMAL: {
    name: 'mySimpleJob',
    config: {
      executor: 'PgBoss',
      perform: PERFORM.MINIMAL,
    } satisfies MinimalConfig<UserApi.JobConfig>,
  },
} as const

export const APP = {
  FULL: {
    name: 'todoApp',
    config: {
      title: 'Todo App',
      wasp: { version: '0.16.3' },
      head: ['<link rel="icon" href="/favicon.ico" />'],
    } satisfies FullConfig<UserApi.AppConfig>,
  },
  MINIMAL: {
    name: 'minimalApp',
    config: {
      title: 'Minimal App',
      wasp: { version: '0.16.3' },
    } satisfies MinimalConfig<UserApi.AppConfig>,
  },
} as const

export const EMAIL_VERIFICATION = {
  FULL: {
    getEmailContentFn: getExtImport('full'),
    clientRoute: ROUTES.EMAIL_VERIFICATION.name,
  } satisfies FullConfig<UserApi.EmailVerificationConfig>,
  MINIMAL: {
    clientRoute: ROUTES.EMAIL_VERIFICATION.name,
  } satisfies MinimalConfig<UserApi.EmailVerificationConfig>,
} as const

export const PASSWORD_RESET = {
  FULL: {
    getEmailContentFn: getExtImport('full'),
    clientRoute: ROUTES.PASSWORD_RESET.name,
  } satisfies FullConfig<UserApi.PasswordResetConfig>,
  MINIMAL: {
    clientRoute: ROUTES.PASSWORD_RESET.name,
  } satisfies MinimalConfig<UserApi.PasswordResetConfig>,
} as const

export const EMAIL_AUTH = {
  FULL: {
    userSignupFields: getExtImport('full'),
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
    userSignupFields: getExtImport('full'),
  } satisfies FullConfig<UserApi.UsernameAndPasswordConfig>,
  MINIMAL: {} satisfies MinimalConfig<UserApi.UsernameAndPasswordConfig>,
} as const

export const EXTERNAL_AUTH = {
  FULL: {
    configFn: getExtImport('full'),
    userSignupFields: getExtImport('full'),
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
    onBeforeSignup: getExtImport('full'),
    onAfterSignup: getExtImport('full'),
    onBeforeOAuthRedirect: getExtImport('full'),
    onBeforeLogin: getExtImport('full'),
    onAfterLogin: getExtImport('full'),
  } satisfies FullConfig<UserApi.AuthConfig>,
  MINIMAL: {
    userEntity: USER_ENTITY,
    onAuthFailedRedirectTo: '/login',
    methods: AUTH_METHODS.MINIMAL,
  } satisfies MinimalConfig<UserApi.AuthConfig>,
} as const

export const CLIENT = {
  FULL: {
    rootComponent: getExtImport('full'),
    setupFn: getExtImport('full'),
    baseDir: '/src',
    envValidationSchema: getExtImport('full'),
  } satisfies FullConfig<UserApi.ClientConfig>,
  MINIMAL: {} satisfies MinimalConfig<UserApi.ClientConfig>,
} as const

export const SERVER = {
  FULL: {
    setupFn: getExtImport('full'),
    middlewareConfigFn: getExtImport('full'),
    envValidationSchema: getExtImport('full'),
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
    fn: getExtImport('full'),
    autoConnect: true,
  } satisfies FullConfig<UserApi.WebsocketConfig>,
  MINIMAL: {
    fn: getExtImport('minimal'),
  } satisfies MinimalConfig<UserApi.WebsocketConfig>,
} as const

export const DB = {
  FULL: {
    seeds: [getExtImport('full', 'named'), getExtImport('full', 'default')],
  } satisfies FullConfig<UserApi.DbConfig>,
  MINIMAL: {} satisfies MinimalConfig<UserApi.DbConfig>,
} as const

function getExtImport(
  scope: 'minimal',
  type?: AppSpec.ExtImportKind
): MinimalConfig<UserApi.ExtImport>
function getExtImport(
  scope: 'full',
  type?: AppSpec.ExtImportKind
): FullConfig<UserApi.ExtImport>
function getExtImport(
  scope: ConfigType,
  type: AppSpec.ExtImportKind | undefined = 'named'
): MinimalConfig<UserApi.ExtImport> | FullConfig<UserApi.ExtImport> {
  const base = { from: '@src/external' } as const

  if (type === 'default') {
    return { ...base, importDefault: 'defaultExport' } satisfies
      | MinimalConfig<UserApi.ExtImport>
      | FullConfig<UserApi.ExtImport>
  }

  if (type === 'named') {
    return { ...base, import: 'namedExport' } satisfies
      | MinimalConfig<UserApi.ExtImport>
      | FullConfig<UserApi.ExtImport>
  }

  throw new Error(`Unhandled scope or type: scope=${scope}, type=${type}`)
}
