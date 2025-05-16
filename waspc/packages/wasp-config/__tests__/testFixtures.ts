/**
 * This module contains sample data that can be used for testing purposes.
 * In our case the sample data represents UserSpec data.
 */

import * as AppSpec from '../src/appSpec.js'
import { Branded } from '../src/branded.js'
import { App } from '../src/publicApi/App.js'
import * as TsAppSpec from '../src/publicApi/tsAppSpec.js'

/**
 * Creates a type containing only the required properties from T.
 *
 * This utility:
 * - Filters out optional properties from the type
 * - Provides a clean type for minimal configuration objects
 * - Returns `Record<string, never>` (empty object) when no required properties exist
 *
 * @template T - The type to extract required properties from
 * @see https://www.totaltypescript.com/the-empty-object-type-in-typescript
 */
type MinimalConfig<T> = keyof T extends never
  ? Record<string, never>
  : {
        [K in keyof T as undefined extends T[K] ? never : K]: T[K]
      } extends infer R
    ? keyof R extends never
      ? Record<string, never>
      : R
    : never

/**
 * Creates a type with all properties and nested properties required.
 *
 * This utility:
 * - Makes all properties required recursively (removes optional flags)
 * - Stops from unwrapping branded types fully
 *
 * @template T - The type to make fully required
 */
type FullConfig<T> =
  T extends Branded<infer U, infer B>
    ? Branded<U, B>
    : T extends object
      ? { [K in keyof T]-?: FullConfig<T[K]> }
      : T

export type Config<T> = MinimalConfig<T> | FullConfig<T>

type MinimalNamedConfig<T> = {
  name: string
  config: MinimalConfig<T>
}

type FullNamedConfig<T> = {
  name: string
  config: FullConfig<T>
}

type NamedConfig<T> = MinimalNamedConfig<T> | FullNamedConfig<T>

const CONFIG_TYPES = ['minimal', 'full'] as const
type ConfigType = (typeof CONFIG_TYPES)[number]

const PAGE_TYPES = [...CONFIG_TYPES, 'email-verification', 'password-reset'] as const
type PageType = (typeof PAGE_TYPES)[number]

const ENTITY_TYPES = ['task', 'user', 'social-user'] as const
type EntityType = (typeof ENTITY_TYPES)[number]

export function createApp(scope: ConfigType): {
  appName: string
  userApp: App
} {
  if (scope === 'minimal') {
    const { name: appName, config: appConfig } = getApp(scope)
    const userApp = new App(appName, appConfig)
    return { appName, userApp }
  }

  const { name: appName, config: appConfig } = getApp(scope)
  const userApp = new App(appName, appConfig)

  userApp.auth(getAuth('full'))
  userApp.client(getClient('full'))
  userApp.server(getServer('full'))
  userApp.emailSender(getEmailSender('full'))
  userApp.webSocket(getWebSocket('full'))
  userApp.db(getDb('full'))

  function addDecls(declName: string, nameAndConfigs: NamedConfig<unknown>[]) {
    nameAndConfigs.forEach(({ name, config }) => userApp[declName](name, config))
  }

  addDecls('page', getPages())
  addDecls('route', getRoutes())
  addDecls('query', getQueries())
  addDecls('action', getActions())
  addDecls('crud', getCruds())
  addDecls('apiNamespace', getApiNamespaces())
  addDecls('api', getApis())
  addDecls('job', getJobs())

  return { appName, userApp }
}

export function getApp(scope: 'minimal'): MinimalNamedConfig<TsAppSpec.AppConfig>
export function getApp(scope: 'full'): FullNamedConfig<TsAppSpec.AppConfig>
export function getApp(scope: ConfigType): NamedConfig<TsAppSpec.AppConfig> {
  if (scope === 'minimal') {
    return {
      name: 'MinimalApp',
      config: {
        title: 'Mock App',
        wasp: { version: '^0.16.3' },
      },
    } satisfies MinimalNamedConfig<TsAppSpec.AppConfig>
  }

  return {
    name: 'FullApp',
    config: {
      title: 'Mock App',
      wasp: { version: '^0.16.3' },
      head: ['<link rel="icon" href="/favicon.ico" />'],
    },
  } satisfies FullNamedConfig<TsAppSpec.AppConfig>
}

export function getAuth(scope: 'minimal'): MinimalConfig<TsAppSpec.AuthConfig>
export function getAuth(scope: 'full'): FullConfig<TsAppSpec.AuthConfig>
export function getAuth(scope: ConfigType): Config<TsAppSpec.AuthConfig> {
  if (scope === 'minimal') {
    return {
      userEntity: getEntity('user'),
      onAuthFailedRedirectTo: '/login',
      methods: getAuthMethods(scope),
    } satisfies MinimalConfig<TsAppSpec.AuthConfig>
  }

  return {
    userEntity: getEntity('user'),
    onAuthFailedRedirectTo: '/login',
    methods: getAuthMethods(scope),
    externalAuthEntity: getEntity('social-user'),
    onAuthSucceededRedirectTo: '/profile',
    onBeforeSignup: getExtImport(scope, 'named'),
    onAfterSignup: getExtImport(scope, 'named'),
    onAfterEmailVerified: getExtImport(scope, 'named'),
    onBeforeOAuthRedirect: getExtImport(scope, 'named'),
    onBeforeLogin: getExtImport(scope, 'named'),
    onAfterLogin: getExtImport(scope, 'named'),
  } satisfies FullConfig<TsAppSpec.AuthConfig>
}

export function getAuthMethods(scope: 'minimal'): MinimalConfig<TsAppSpec.AuthMethods>
export function getAuthMethods(scope: 'full'): FullConfig<TsAppSpec.AuthMethods>
export function getAuthMethods(scope: ConfigType): Config<TsAppSpec.AuthMethods> {
  if (scope === 'minimal') {
    return {} satisfies MinimalConfig<TsAppSpec.AuthMethods>
  }

  return {
    email: getEmailAuth(scope),
    usernameAndPassword: getUsernameAndPassword(scope),
    discord: getExternalAuth(scope),
    google: getExternalAuth(scope),
    gitHub: getExternalAuth(scope),
    keycloak: getExternalAuth(scope),
  } satisfies FullConfig<TsAppSpec.AuthMethods>
}

export function getExternalAuth(scope: 'minimal'): MinimalConfig<TsAppSpec.ExternalAuthConfig>
export function getExternalAuth(scope: 'full'): FullConfig<TsAppSpec.ExternalAuthConfig>
export function getExternalAuth(scope: ConfigType): Config<TsAppSpec.ExternalAuthConfig> {
  if (scope === 'minimal') {
    return {} satisfies MinimalConfig<TsAppSpec.ExternalAuthConfig>
  }

  return {
    configFn: getExtImport(scope, 'named'),
    userSignupFields: getExtImport(scope, 'named'),
  } satisfies FullConfig<TsAppSpec.ExternalAuthConfig>
}

export function getUsernameAndPassword(
  scope: 'minimal'
): MinimalConfig<TsAppSpec.UsernameAndPasswordConfig>
export function getUsernameAndPassword(
  scope: 'full'
): FullConfig<TsAppSpec.UsernameAndPasswordConfig>
export function getUsernameAndPassword(
  scope: ConfigType
): Config<TsAppSpec.UsernameAndPasswordConfig> {
  if (scope === 'minimal') {
    return {} satisfies MinimalConfig<TsAppSpec.UsernameAndPasswordConfig>
  }

  return {
    userSignupFields: getExtImport(scope, 'named'),
  } satisfies FullConfig<TsAppSpec.UsernameAndPasswordConfig>
}

export function getEmailAuth(scope: 'minimal'): MinimalConfig<TsAppSpec.EmailAuthConfig>
export function getEmailAuth(scope: 'full'): FullConfig<TsAppSpec.EmailAuthConfig>
export function getEmailAuth(scope: ConfigType): Config<TsAppSpec.EmailAuthConfig> {
  if (scope === 'minimal') {
    return {
      fromField: getEmailFromField(scope),
      emailVerification: getEmailVerification(scope),
      passwordReset: getPasswordReset(scope),
    } satisfies MinimalConfig<TsAppSpec.EmailAuthConfig>
  }

  return {
    fromField: getEmailFromField(scope),
    emailVerification: getEmailVerification(scope),
    passwordReset: getPasswordReset(scope),
    userSignupFields: getExtImport(scope, 'named'),
  } satisfies FullConfig<TsAppSpec.EmailAuthConfig>
}

export function getPasswordReset(scope: 'minimal'): MinimalConfig<TsAppSpec.PasswordResetConfig>
export function getPasswordReset(scope: 'full'): FullConfig<TsAppSpec.PasswordResetConfig>
export function getPasswordReset(scope: ConfigType): Config<TsAppSpec.PasswordResetConfig> {
  if (scope === 'minimal') {
    return {
      clientRoute: getRoute('password-reset').name,
    } satisfies MinimalConfig<TsAppSpec.PasswordResetConfig>
  }

  return {
    clientRoute: getRoute('password-reset').name,
    getEmailContentFn: getExtImport(scope, 'named'),
  } satisfies FullConfig<TsAppSpec.PasswordResetConfig>
}

export function getEmailVerification(
  scope: 'minimal'
): MinimalConfig<TsAppSpec.EmailVerificationConfig>
export function getEmailVerification(scope: 'full'): FullConfig<TsAppSpec.EmailVerificationConfig>
export function getEmailVerification(scope: ConfigType): Config<TsAppSpec.EmailVerificationConfig> {
  if (scope === 'minimal') {
    return {
      clientRoute: getRoute('email-verification').name,
    } satisfies MinimalConfig<TsAppSpec.EmailVerificationConfig>
  }

  return {
    clientRoute: getRoute('email-verification').name,
    getEmailContentFn: getExtImport(scope, 'named'),
  } satisfies FullConfig<TsAppSpec.EmailVerificationConfig>
}

export function getClient(scope: 'minimal'): MinimalConfig<TsAppSpec.ClientConfig>
export function getClient(scope: 'full'): FullConfig<TsAppSpec.ClientConfig>
export function getClient(scope: ConfigType): Config<TsAppSpec.ClientConfig> {
  if (scope === 'minimal') {
    return {} satisfies MinimalConfig<TsAppSpec.ClientConfig>
  }

  return {
    rootComponent: getExtImport(scope, 'named'),
    setupFn: getExtImport(scope, 'named'),
    baseDir: '/src',
    envValidationSchema: getExtImport(scope, 'named'),
  } satisfies FullConfig<TsAppSpec.ClientConfig>
}

export function getServer(scope: 'minimal'): MinimalConfig<TsAppSpec.ServerConfig>
export function getServer(scope: 'full'): FullConfig<TsAppSpec.ServerConfig>
export function getServer(scope: ConfigType): Config<TsAppSpec.ServerConfig> {
  if (scope === 'minimal') {
    return {} satisfies MinimalConfig<TsAppSpec.ServerConfig>
  }

  return {
    setupFn: getExtImport(scope, 'named'),
    middlewareConfigFn: getExtImport(scope, 'named'),
    envValidationSchema: getExtImport(scope, 'named'),
  } satisfies FullConfig<TsAppSpec.ServerConfig>
}

export function getEmailSender(scope: 'minimal'): MinimalConfig<TsAppSpec.EmailSenderConfig>
export function getEmailSender(scope: 'full'): FullConfig<TsAppSpec.EmailSenderConfig>
export function getEmailSender(scope: ConfigType): Config<TsAppSpec.EmailSenderConfig> {
  if (scope === 'minimal') {
    return {
      provider: 'SMTP',
    } satisfies MinimalConfig<TsAppSpec.EmailSenderConfig>
  }

  return {
    provider: 'SMTP',
    defaultFrom: getEmailFromField(scope),
  } satisfies FullConfig<TsAppSpec.EmailSenderConfig>
}

export function getWebSocket(scope: 'minimal'): MinimalConfig<TsAppSpec.WebsocketConfig>
export function getWebSocket(scope: 'full'): FullConfig<TsAppSpec.WebsocketConfig>
export function getWebSocket(scope: ConfigType): Config<TsAppSpec.WebsocketConfig> {
  if (scope === 'minimal') {
    return {
      fn: getExtImport(scope, 'named'),
    } satisfies MinimalConfig<TsAppSpec.WebsocketConfig>
  }

  return {
    fn: getExtImport(scope, 'named'),
    autoConnect: true,
  } satisfies FullConfig<TsAppSpec.WebsocketConfig>
}

export function getDb(scope: 'minimal'): MinimalConfig<TsAppSpec.DbConfig>
export function getDb(scope: 'full'): FullConfig<TsAppSpec.DbConfig>
export function getDb(scope: ConfigType): Config<TsAppSpec.DbConfig> {
  if (scope === 'minimal') {
    return {} satisfies MinimalConfig<TsAppSpec.DbConfig>
  }

  return {
    seeds: [getExtImport(scope, 'named'), getExtImport(scope, 'default')],
  } satisfies FullConfig<TsAppSpec.DbConfig>
}

export function getPages(): NamedConfig<TsAppSpec.PageConfig>[] {
  // @ts-expect-error we don't need to know the specifc function overload in this case
  return PAGE_TYPES.map((pageType) => getPage(pageType))
}

export function getPage(pageType: 'minimal'): MinimalNamedConfig<TsAppSpec.PageConfig>
export function getPage(
  pageType: 'full' | 'email-verification' | 'password-reset'
): FullNamedConfig<TsAppSpec.PageConfig>
export function getPage(pageType: PageType): NamedConfig<TsAppSpec.PageConfig> {
  if (pageType === 'minimal') {
    return {
      name: 'MinimalPage',
      config: {
        component: getExtImport(pageType, 'named'),
      },
    } satisfies MinimalNamedConfig<TsAppSpec.PageConfig>
  } else if (pageType === 'email-verification') {
    return {
      name: 'EmailVerificationPage',
      config: {
        component: getExtImport('full', 'named'),
        authRequired: false,
      },
    } satisfies FullNamedConfig<TsAppSpec.PageConfig>
  } else if (pageType === 'password-reset') {
    return {
      name: 'PasswordResetPage',
      config: {
        component: getExtImport('full', 'named'),
        authRequired: false,
      },
    } satisfies FullNamedConfig<TsAppSpec.PageConfig>
  }

  return {
    name: 'FullPage',
    config: {
      component: getExtImport(pageType, 'named'),
      authRequired: true,
    },
  } satisfies NamedConfig<TsAppSpec.PageConfig>
}

export function getRoutes(): NamedConfig<TsAppSpec.RouteConfig>[] {
  // @ts-expect-error we don't need to know the specifc function overload in this case
  return PAGE_TYPES.map((pageType) => getRoute(pageType))
}

export function getRoute(routeType: 'minimal'): MinimalNamedConfig<TsAppSpec.RouteConfig>
export function getRoute(
  routeType: 'full' | 'email-verification' | 'password-reset'
): FullNamedConfig<TsAppSpec.RouteConfig>
export function getRoute(routeType: PageType): NamedConfig<TsAppSpec.RouteConfig> {
  if (routeType === 'minimal') {
    return {
      name: 'MinimalRoute',
      config: {
        path: '/foo/bar',
        to: getPage(routeType).name as TsAppSpec.PageName,
      },
    } satisfies MinimalNamedConfig<TsAppSpec.RouteConfig>
  }

  let name: string
  if (routeType === 'email-verification') {
    name = 'EmailVerificationRoute'
  } else if (routeType === 'password-reset') {
    name = 'PasswordResetRoute'
  } else {
    name = 'FullRoute'
  }

  return {
    name,
    config: {
      path: '/foo/bar',
      to: getPage(routeType).name as TsAppSpec.PageName,
    },
  } satisfies FullNamedConfig<TsAppSpec.RouteConfig>
}

export function getQueries(): NamedConfig<TsAppSpec.QueryConfig>[] {
  // @ts-expect-error we don't need to know the specifc function overload in this case
  return CONFIG_TYPES.map((scope) => getQuery(scope))
}

export function getQuery(scope: 'minimal'): MinimalNamedConfig<TsAppSpec.QueryConfig>
export function getQuery(scope: 'full'): FullNamedConfig<TsAppSpec.QueryConfig>
export function getQuery(scope: ConfigType): NamedConfig<TsAppSpec.QueryConfig> {
  if (scope === 'minimal') {
    return {
      name: 'MinimalQuery',
      config: {
        fn: getExtImport(scope, 'named'),
      },
    } satisfies MinimalNamedConfig<TsAppSpec.QueryConfig>
  }

  return {
    name: 'FullQuery',
    config: {
      fn: getExtImport(scope, 'named'),
      entities: [getEntity('task')],
      auth: true,
    },
  } satisfies FullNamedConfig<TsAppSpec.QueryConfig>
}

export function getActions(): NamedConfig<TsAppSpec.ActionConfig>[] {
  // @ts-expect-error we don't need to know the specifc function overload in this case
  return CONFIG_TYPES.map((scope) => getAction(scope))
}

export function getAction(scope: 'minimal'): MinimalNamedConfig<TsAppSpec.ActionConfig>
export function getAction(scope: 'full'): FullNamedConfig<TsAppSpec.ActionConfig>
export function getAction(scope: ConfigType): NamedConfig<TsAppSpec.ActionConfig> {
  if (scope === 'minimal') {
    return {
      name: 'MinimalAction',
      config: {
        fn: getExtImport(scope, 'named'),
      },
    } satisfies MinimalNamedConfig<TsAppSpec.ActionConfig>
  }

  return {
    name: 'FullAction',
    config: {
      fn: getExtImport(scope, 'named'),
      entities: [getEntity('task')],
      auth: true,
    },
  } satisfies FullNamedConfig<TsAppSpec.ActionConfig>
}

export function getCruds(): NamedConfig<TsAppSpec.CrudConfig>[] {
  // @ts-expect-error we don't need to know the specifc function overload in this case
  return CONFIG_TYPES.map((scope) => getCrud(scope))
}

export function getCrud(scope: 'minimal'): MinimalNamedConfig<TsAppSpec.CrudConfig>
export function getCrud(scope: 'full'): FullNamedConfig<TsAppSpec.CrudConfig>
export function getCrud(scope: ConfigType): NamedConfig<TsAppSpec.CrudConfig> {
  if (scope === 'minimal') {
    return {
      name: 'MinimalCrud',
      config: {
        entity: getEntity('task'),
        operations: getCrudOperations(scope),
      },
    } satisfies MinimalNamedConfig<TsAppSpec.CrudConfig>
  }

  return {
    name: 'FullCrud',
    config: {
      entity: getEntity('task'),
      operations: getCrudOperations(scope),
    },
  } satisfies FullNamedConfig<TsAppSpec.CrudConfig>
}

export function getCrudOperations(scope: 'minimal'): MinimalConfig<TsAppSpec.CrudOperations>
export function getCrudOperations(scope: 'full'): FullConfig<TsAppSpec.CrudOperations>
export function getCrudOperations(scope: ConfigType): Config<TsAppSpec.CrudOperations> {
  if (scope === 'minimal') {
    return {} satisfies MinimalConfig<TsAppSpec.CrudOperations>
  }

  return {
    get: getCrudOperationOptions(scope),
    getAll: getCrudOperationOptions(scope),
    create: getCrudOperationOptions(scope),
    update: getCrudOperationOptions(scope),
    delete: getCrudOperationOptions(scope),
  } satisfies FullConfig<TsAppSpec.CrudOperations>
}

export function getCrudOperationOptions(
  scope: 'minimal'
): MinimalConfig<TsAppSpec.CrudOperationOptions>
export function getCrudOperationOptions(scope: 'full'): FullConfig<TsAppSpec.CrudOperationOptions>
export function getCrudOperationOptions(scope: ConfigType): Config<TsAppSpec.CrudOperationOptions> {
  if (scope === 'minimal') {
    return {} satisfies MinimalConfig<TsAppSpec.CrudOperationOptions>
  }

  return {
    isPublic: true,
    overrideFn: getExtImport(scope, 'named'),
  } satisfies FullConfig<TsAppSpec.CrudOperationOptions>
}

export function getSchedule(scope: 'minimal'): MinimalConfig<TsAppSpec.ScheduleConfig>
export function getSchedule(scope: 'full'): FullConfig<TsAppSpec.ScheduleConfig>
export function getSchedule(scope: ConfigType): Config<TsAppSpec.ScheduleConfig> {
  if (scope === 'minimal') {
    return {
      cron: '0 0 * * *',
    } satisfies MinimalConfig<TsAppSpec.ScheduleConfig>
  }

  return {
    cron: '0 0 * * *',
    args: { foo: 'bar' },
    executorOptions: {
      pgBoss: { jobOptions: { attempts: 3 } },
    },
  } satisfies FullConfig<TsAppSpec.ScheduleConfig>
}

export function getPerform(scope: 'minimal'): MinimalConfig<TsAppSpec.Perform>
export function getPerform(scope: 'full'): FullConfig<TsAppSpec.Perform>
export function getPerform(
  scope: ConfigType
): MinimalConfig<TsAppSpec.Perform> | FullConfig<TsAppSpec.Perform> {
  if (scope === 'minimal') {
    return {
      fn: getExtImport(scope, 'named'),
    } satisfies MinimalConfig<TsAppSpec.Perform>
  }

  return {
    fn: getExtImport(scope, 'named'),
    executorOptions: {
      pgBoss: { jobOptions: { attempts: 3 } },
    },
  } satisfies FullConfig<TsAppSpec.Perform>
}

export function getApiNamespaces(): NamedConfig<TsAppSpec.ApiNamespaceConfig>[] {
  // @ts-expect-error we don't need to know the specifc function overload in this case
  return CONFIG_TYPES.map((scope) => getApiNamespace(scope))
}

export function getApiNamespace(scope: 'minimal'): MinimalNamedConfig<TsAppSpec.ApiNamespaceConfig>
export function getApiNamespace(scope: 'full'): FullNamedConfig<TsAppSpec.ApiNamespaceConfig>
export function getApiNamespace(scope: ConfigType): NamedConfig<TsAppSpec.ApiNamespaceConfig> {
  if (scope === 'minimal') {
    return {
      name: 'MinimalApiNamespace',
      config: {
        middlewareConfigFn: getExtImport(scope, 'named'),
        path: '/foo',
      },
    } satisfies MinimalNamedConfig<TsAppSpec.ApiNamespaceConfig>
  }

  return {
    name: 'FullApiNamespace',
    config: {
      middlewareConfigFn: getExtImport(scope, 'named'),
      path: '/foo',
    },
  } satisfies FullNamedConfig<TsAppSpec.ApiNamespaceConfig>
}

export function getApis(): NamedConfig<TsAppSpec.ApiConfig>[] {
  // @ts-expect-error we don't need to know the specifc function overload in this case
  return CONFIG_TYPES.map((scope) => getApi(scope))
}

export function getApi(scope: 'minimal'): MinimalNamedConfig<TsAppSpec.ApiConfig>
export function getApi(scope: 'full'): FullNamedConfig<TsAppSpec.ApiConfig>
export function getApi(scope: ConfigType): NamedConfig<TsAppSpec.ApiConfig> {
  if (scope === 'minimal') {
    return {
      name: 'MinimalApi',
      config: {
        fn: getExtImport(scope, 'named'),
        httpRoute: getHttpRoute(scope),
      },
    } satisfies MinimalNamedConfig<TsAppSpec.ApiConfig>
  }

  return {
    name: 'FullApi',
    config: {
      fn: getExtImport(scope, 'named'),
      httpRoute: getHttpRoute(scope),
      entities: [getEntity('task')],
      auth: true,
      middlewareConfigFn: getExtImport(scope, 'named'),
    },
  } satisfies FullNamedConfig<TsAppSpec.ApiConfig>
}

export function getHttpRoute(scope: 'minimal'): MinimalConfig<TsAppSpec.HttpRoute>
export function getHttpRoute(scope: 'full'): FullConfig<TsAppSpec.HttpRoute>
export function getHttpRoute(scope: ConfigType): Config<TsAppSpec.HttpRoute> {
  if (scope === 'minimal') {
    return {
      method: 'GET',
      route: '/foo/bar',
    } satisfies MinimalConfig<TsAppSpec.HttpRoute>
  }

  return {
    method: 'GET',
    route: '/foo/bar',
  } satisfies FullConfig<TsAppSpec.HttpRoute>
}

export function getJobs(): NamedConfig<TsAppSpec.JobConfig>[] {
  // @ts-expect-error we don't need to know the specifc function overload in this case
  return CONFIG_TYPES.map((scope) => getJob(scope))
}

export function getJob(scope: 'minimal'): MinimalNamedConfig<TsAppSpec.JobConfig>
export function getJob(scope: 'full'): FullNamedConfig<TsAppSpec.JobConfig>
export function getJob(scope: ConfigType): NamedConfig<TsAppSpec.JobConfig> {
  if (scope === 'minimal') {
    return {
      name: 'MinimalJob',
      config: {
        executor: 'PgBoss',
        perform: getPerform(scope),
      },
    } satisfies MinimalNamedConfig<TsAppSpec.JobConfig>
  }

  return {
    name: 'FullJob',
    config: {
      executor: 'PgBoss',
      perform: getPerform(scope),
      entities: [getEntity('task')],
      schedule: getSchedule(scope),
    },
  } satisfies FullNamedConfig<TsAppSpec.JobConfig>
}

export function getEmailFromField(scope: 'minimal'): MinimalConfig<TsAppSpec.EmailFromField>
export function getEmailFromField(scope: 'full'): FullConfig<TsAppSpec.EmailFromField>
export function getEmailFromField(scope: ConfigType): Config<TsAppSpec.EmailFromField> {
  if (scope === 'minimal') {
    return {
      email: 'test@domain.tld',
    } satisfies MinimalConfig<TsAppSpec.EmailFromField>
  }

  return {
    email: 'test@domain.tld',
    name: 'ToDo App',
  } satisfies FullConfig<TsAppSpec.EmailFromField>
}

export function getExtImport(
  scope: 'minimal',
  type: AppSpec.ExtImportKind
): MinimalConfig<TsAppSpec.ExtImport>
export function getExtImport(
  scope: 'full',
  type: AppSpec.ExtImportKind
): FullConfig<TsAppSpec.ExtImport>
export function getExtImport(
  scope: ConfigType,
  type: AppSpec.ExtImportKind
): Config<TsAppSpec.ExtImport> {
  if (type === 'default') {
    if (scope === 'minimal') {
      return {
        from: '@src/external',
        importDefault: 'defaultExport',
      } satisfies MinimalConfig<TsAppSpec.ExtImport>
    }

    return {
      from: '@src/external',
      importDefault: 'defaultExport',
    } satisfies FullConfig<TsAppSpec.ExtImport>
  }

  if (type === 'named') {
    if (scope === 'minimal') {
      return {
        from: '@src/external',
        import: 'namedExport',
      } satisfies MinimalConfig<TsAppSpec.ExtImport>
    }

    return {
      from: '@src/external',
      import: 'namedExport',
    } satisfies FullConfig<TsAppSpec.ExtImport>
  }

  throw new Error(`Unhandled scope or type: scope=${scope}, type=${type}`)
}

export function getEntity(entity: EntityType) {
  switch (entity) {
    case 'task':
      return 'Task'
    case 'user':
      return 'User'
    case 'social-user':
      return 'SocialUser'
    default:
      throw new Error(`Unknown entity: ${entity}`)
  }
}

export function getEntities(scope: ConfigType): string[] {
  if (scope === 'minimal') {
    return []
  }

  return ENTITY_TYPES.map((entity) => getEntity(entity))
}
