/**
 * This module contains sample data that can be used for testing purposes.
 * In our case the sample data represents UserSpec data.
 */

import * as AppSpec from '../src/appSpec.js'
import * as UserApi from '../src/userApi.js'

/**
 * This type removes all properties from T that are optional.
 *
 * The empty object type - {} - doesn't behave how you expect in TypeScript.
 * It represents any value except null and undefined.
 * To represent empty objects, we use `Record<string, never>` instead.
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
 * Required alias for domain consistency sake.
 */
type FullConfig<T> = Required<T>

export type Config<T> = MinimalConfig<T> | FullConfig<T>
export type NamedConfig<T> = {
  name: string
  config: Config<T>
}

type ConfigType = 'minimal' | 'full'

export function getEntity(entity: 'task' | 'user' | 'social-user') {
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

export function getEntities(): string[] {
  return [getEntity('task'), getEntity('user'), getEntity('social-user')]
}

export function createUserApp(scope: ConfigType): UserApi.App {
  if (scope === 'minimal') {
    return createMinimalUserApp()
  } else if (scope === 'full') {
    return createFullUserApp()
  } else {
    throw new Error(`Unknown config type: ${scope}`)
  }
}

function createMinimalUserApp(): UserApi.App {
  const { name, config } = getApp('minimal')
  return new UserApi.App(name, config)
}

function createFullUserApp(): UserApi.App {
  const { name, config } = getApp('full')
  const app = new UserApi.App(name, config)
  app.auth(getAuth('full'))
  app.client(getClient('full'))
  app.server(getServer('full'))
  app.emailSender(getEmailSender('full'))
  app.webSocket(getWebSocket('full'))
  app.db(getDb('full'))

  function addDecls(declName: string, nameAndConfigs: NamedConfig<unknown>[]) {
    nameAndConfigs.forEach(({ name, config }) => app[declName](name, config))
  }

  addDecls('page', getPages())
  addDecls('route', getRoutes())
  addDecls('query', getQueries())
  addDecls('action', getActions())
  addDecls('crud', getCruds())
  addDecls('apiNamespace', getApiNamespaces())
  addDecls('api', getApis())
  addDecls('job', getJobs())

  return app
}

export function getApp(scope: ConfigType): NamedConfig<UserApi.AppConfig> {
  const minimal: MinimalConfig<UserApi.AppConfig> = {
    title: 'Mock App',
    wasp: { version: '^0.16.3' },
  }

  return {
    name: scope === 'minimal' ? 'MinimalApp' : 'FullApp',
    config:
      scope === 'minimal'
        ? minimal
        : ({
            ...minimal,
            head: ['<link rel="icon" href="/favicon.ico" />'],
          } satisfies FullConfig<UserApi.AppConfig>),
  }
}

export function getAuth(scope: ConfigType): Config<UserApi.AuthConfig> {
  const minimal: MinimalConfig<UserApi.AuthConfig> = {
    userEntity: getEntity('user'),
    onAuthFailedRedirectTo: '/login',
    methods: getAuthMethods(scope),
  }

  return scope === 'minimal'
    ? minimal
    : ({
        ...minimal,
        externalAuthEntity: getEntity('social-user'),
        onAuthSucceededRedirectTo: '/profile',
        onBeforeSignup: getExtImport(scope, 'named'),
        onAfterSignup: getExtImport(scope, 'named'),
        onAfterEmailVerified: getExtImport(scope, 'named'),
        onBeforeOAuthRedirect: getExtImport(scope, 'named'),
        onBeforeLogin: getExtImport(scope, 'named'),
        onAfterLogin: getExtImport(scope, 'named'),
      } satisfies FullConfig<UserApi.AuthConfig>)
}

export function getAuthMethods(scope: ConfigType): Config<UserApi.AuthMethods> {
  const minimal: MinimalConfig<UserApi.AuthMethods> = {}

  return scope === 'minimal'
    ? minimal
    : ({
        ...minimal,
        email: getEmailAuth(scope),
        usernameAndPassword: getUsernameAndPassword(scope),
        discord: getExternalAuth(scope),
        google: getExternalAuth(scope),
        gitHub: getExternalAuth(scope),
        keycloak: getExternalAuth(scope),
      } satisfies FullConfig<UserApi.AuthMethods>)
}

export function getExternalAuth(
  scope: ConfigType
): Config<UserApi.ExternalAuthConfig> {
  const minimal: MinimalConfig<UserApi.ExternalAuthConfig> = {}

  return scope === 'minimal'
    ? minimal
    : ({
        ...minimal,
        configFn: getExtImport(scope, 'named'),
        userSignupFields: getExtImport(scope, 'named'),
      } satisfies FullConfig<UserApi.ExternalAuthConfig>)
}

export function getUsernameAndPassword(
  scope: ConfigType
): Config<UserApi.UsernameAndPasswordConfig> {
  const minimal: MinimalConfig<UserApi.UsernameAndPasswordConfig> = {}

  return scope === 'minimal'
    ? minimal
    : ({
        ...minimal,
        userSignupFields: getExtImport(scope, 'named'),
      } satisfies FullConfig<UserApi.UsernameAndPasswordConfig>)
}

export function getEmailAuth(
  scope: ConfigType
): Config<UserApi.EmailAuthConfig> {
  const minimal: MinimalConfig<UserApi.EmailAuthConfig> = {
    fromField: getEmailFromField(scope),
    emailVerification: getEmailVerification(scope),
    passwordReset: getPasswordReset(scope),
  }

  return scope === 'minimal'
    ? minimal
    : ({
        ...minimal,
        userSignupFields: getExtImport(scope, 'named'),
      } satisfies FullConfig<UserApi.EmailAuthConfig>)
}

export function getPasswordReset(
  scope: ConfigType
): Config<UserApi.PasswordResetConfig> {
  const minimal: MinimalConfig<UserApi.PasswordResetConfig> = {
    clientRoute: getRoute('password-reset').name,
  }

  return scope === 'minimal'
    ? minimal
    : ({
        ...minimal,
        getEmailContentFn: getExtImport(scope, 'named'),
      } satisfies FullConfig<UserApi.PasswordResetConfig>)
}

export function getEmailVerification(
  scope: ConfigType
): Config<UserApi.EmailVerificationConfig> {
  const minimal: MinimalConfig<UserApi.EmailVerificationConfig> = {
    clientRoute: getRoute('email-verification').name,
  }

  return scope === 'minimal'
    ? minimal
    : ({
        ...minimal,
        getEmailContentFn: getExtImport(scope, 'named'),
      } satisfies FullConfig<UserApi.EmailVerificationConfig>)
}

export function getClient(scope: ConfigType): Config<UserApi.ClientConfig> {
  const minimal: MinimalConfig<UserApi.ClientConfig> = {}

  return scope === 'minimal'
    ? minimal
    : ({
        ...minimal,
        rootComponent: getExtImport(scope, 'named'),
        setupFn: getExtImport(scope, 'named'),
        baseDir: '/src',
        envValidationSchema: getExtImport(scope, 'named'),
      } satisfies FullConfig<UserApi.ClientConfig>)
}

export function getServer(scope: ConfigType): Config<UserApi.ServerConfig> {
  const minimal: MinimalConfig<UserApi.ServerConfig> = {}

  return scope === 'minimal'
    ? minimal
    : ({
        ...minimal,
        setupFn: getExtImport(scope, 'named'),
        middlewareConfigFn: getExtImport(scope, 'named'),
        envValidationSchema: getExtImport(scope, 'named'),
      } satisfies FullConfig<UserApi.ServerConfig>)
}

export function getEmailSender(
  scope: ConfigType
): Config<UserApi.EmailSenderConfig> {
  const minimal: MinimalConfig<UserApi.EmailSenderConfig> = {
    provider: 'SMTP',
  }

  return scope === 'minimal'
    ? minimal
    : ({
        ...minimal,
        defaultFrom: getEmailFromField(scope),
      } satisfies FullConfig<UserApi.EmailSenderConfig>)
}

export function getWebSocket(
  scope: ConfigType
): Config<UserApi.WebsocketConfig> {
  const minimal: MinimalConfig<UserApi.WebsocketConfig> = {
    fn: getExtImport(scope, 'named'),
  }

  return scope === 'minimal'
    ? minimal
    : ({
        ...minimal,
        autoConnect: true,
      } satisfies FullConfig<UserApi.WebsocketConfig>)
}

export function getDb(scope: ConfigType): Config<UserApi.DbConfig> {
  const minimal: MinimalConfig<UserApi.DbConfig> = {}

  return scope === 'minimal'
    ? minimal
    : ({
        ...minimal,
        seeds: [getExtImport(scope, 'named'), getExtImport(scope, 'default')],
      } satisfies FullConfig<UserApi.DbConfig>)
}

export function getEmailFromField(
  scope: ConfigType
): Config<UserApi.EmailFromField> {
  const minimal: MinimalConfig<UserApi.EmailFromField> = {
    email: 'test@domain.tld',
  }

  return scope === 'minimal'
    ? minimal
    : ({
        ...minimal,
        name: 'ToDo App',
      } satisfies FullConfig<UserApi.EmailFromField>)
}

export function getExtImport(
  scope: ConfigType,
  type: AppSpec.ExtImportKind
): Config<UserApi.ExtImport> {
  if (type === 'default') {
    const minimal: MinimalConfig<UserApi.ExtImport> = {
      from: '@src/external',
      importDefault: 'defaultExport',
    }

    return scope === 'minimal'
      ? minimal
      : (minimal satisfies FullConfig<UserApi.ExtImport>)
  }

  if (type === 'named') {
    const minimal: MinimalConfig<UserApi.ExtImport> = {
      from: '@src/external',
      import: 'namedExport',
    }

    return scope === 'minimal'
      ? minimal
      : (minimal satisfies FullConfig<UserApi.ExtImport>)
  }

  throw new Error(`Unhandled scope or type: scope=${scope}, type=${type}`)
}

export function getPages(): NamedConfig<UserApi.PageConfig>[] {
  return [
    getPage('minimal'),
    getPage('full'),
    getPage('email-verification'),
    getPage('password-reset'),
  ]
}

export function getPage(
  pageType: ConfigType | 'email-verification' | 'password-reset'
): NamedConfig<UserApi.PageConfig> {
  const scope = pageType === 'full' ? 'full' : 'minimal'

  let name: string
  if (pageType === 'minimal') {
    name = 'MinimalPage'
  } else if (pageType === 'full') {
    name = 'FullPage'
  } else if (pageType === 'email-verification') {
    name = 'EmailVerificationPage'
  } else if (pageType === 'password-reset') {
    name = 'PasswordResetPage'
  } else {
    throw new Error(`Unhandled page type: ${pageType}`)
  }

  const minimal: MinimalConfig<UserApi.PageConfig> = {
    component: getExtImport(scope, 'named'),
  }

  return {
    name,
    config:
      pageType === 'minimal'
        ? minimal
        : ({
            ...minimal,
            authRequired: true,
          } satisfies FullConfig<UserApi.PageConfig>),
  }
}

export function getRoutes(): NamedConfig<UserApi.RouteConfig>[] {
  return [
    getRoute('minimal'),
    getRoute('full'),
    getRoute('email-verification'),
    getRoute('password-reset'),
  ]
}

export function getRoute(
  routeType: ConfigType | 'email-verification' | 'password-reset'
): NamedConfig<UserApi.RouteConfig> {
  let name: string
  if (routeType === 'minimal') {
    name = 'MinimalRoute'
  } else if (routeType === 'full') {
    name = 'FullRoute'
  } else if (routeType === 'email-verification') {
    name = 'EmailVerificationRoute'
  } else if (routeType === 'password-reset') {
    name = 'PasswordResetRoute'
  } else {
    throw new Error(`Unhandled route type: ${routeType}`)
  }

  const minimal: MinimalConfig<UserApi.RouteConfig> = {
    path: '/foo/bar',
    to: getPage(routeType).name as string & { _brand: 'Page' },
  }

  return {
    name,
    config:
      routeType === 'minimal'
        ? minimal
        : (minimal satisfies FullConfig<UserApi.RouteConfig>),
  }
}

export function getQueries(): NamedConfig<UserApi.QueryConfig>[] {
  return [getQuery('minimal'), getQuery('full')]
}

export function getQuery(scope: ConfigType): NamedConfig<UserApi.QueryConfig> {
  const minimal: MinimalConfig<UserApi.QueryConfig> = {
    fn: getExtImport(scope, 'named'),
  }

  return {
    name: scope === 'minimal' ? 'MinimalQuery' : 'FullQuery',
    config:
      scope === 'minimal'
        ? minimal
        : ({
            ...minimal,
            entities: [getEntity('task')],
            auth: true,
          } satisfies FullConfig<UserApi.QueryConfig>),
  }
}

export function getActions(): NamedConfig<UserApi.ActionConfig>[] {
  return [getAction('minimal'), getAction('full')]
}

export function getAction(
  scope: ConfigType
): NamedConfig<UserApi.ActionConfig> {
  const minimal: MinimalConfig<UserApi.ActionConfig> = {
    fn: getExtImport(scope, 'named'),
  }

  return {
    name: scope === 'minimal' ? 'MinimalAction' : 'FullAction',
    config:
      scope === 'minimal'
        ? minimal
        : ({
            ...minimal,
            entities: [getEntity('task')],
            auth: true,
          } satisfies FullConfig<UserApi.ActionConfig>),
  }
}

export function getCruds(): NamedConfig<UserApi.Crud>[] {
  return [getCrud('minimal'), getCrud('full')]
}

export function getCrud(scope: ConfigType): NamedConfig<UserApi.Crud> {
  const minimal: MinimalConfig<UserApi.Crud> = {
    entity: getEntity('task'),
    operations: getCrudOperations(scope),
  }

  return {
    name: scope === 'minimal' ? 'MinimalCrud' : 'FullCrud',
    config:
      scope === 'minimal'
        ? minimal
        : (minimal satisfies FullConfig<UserApi.Crud>),
  }
}

export function getCrudOperations(
  scope: ConfigType
): MinimalConfig<UserApi.CrudOperations> | FullConfig<UserApi.CrudOperations> {
  const minimal: MinimalConfig<UserApi.CrudOperations> = {}

  return scope === 'minimal'
    ? minimal
    : ({
        ...minimal,
        get: getCrudOperationOptions(scope),
        getAll: getCrudOperationOptions(scope),
        create: getCrudOperationOptions(scope),
        update: getCrudOperationOptions(scope),
        delete: getCrudOperationOptions(scope),
      } satisfies FullConfig<UserApi.CrudOperations>)
}

export function getCrudOperationOptions(
  scope: ConfigType
):
  | MinimalConfig<UserApi.CrudOperationOptions>
  | FullConfig<UserApi.CrudOperationOptions> {
  const minimal: MinimalConfig<UserApi.CrudOperationOptions> = {}

  return scope === 'minimal'
    ? minimal
    : ({
        ...minimal,
        isPublic: true,
        overrideFn: getExtImport(scope, 'named'),
      } satisfies FullConfig<UserApi.CrudOperationOptions>)
}

export function getApiNamespaces(): NamedConfig<UserApi.ApiNamespaceConfig>[] {
  return [getApiNamespace('minimal'), getApiNamespace('full')]
}

export function getApiNamespace(
  scope: ConfigType
): NamedConfig<UserApi.ApiNamespaceConfig> {
  const minimal: MinimalConfig<UserApi.ApiNamespaceConfig> = {
    middlewareConfigFn: getExtImport(scope, 'named'),
    path: '/foo',
  }

  return {
    name: scope === 'minimal' ? 'MinimalApiNamespace' : 'FullApiNamespace',
    config:
      scope === 'minimal'
        ? minimal
        : (minimal satisfies FullConfig<UserApi.ApiNamespaceConfig>),
  }
}

export function getHttpRoute(
  scope: ConfigType
): MinimalConfig<UserApi.HttpRoute> | FullConfig<UserApi.HttpRoute> {
  const minimal: MinimalConfig<UserApi.HttpRoute> = {
    method: 'GET',
    route: '/foo/bar',
  }

  return scope === 'minimal'
    ? minimal
    : (minimal satisfies FullConfig<UserApi.HttpRoute>)
}

export function getApis(): NamedConfig<UserApi.ApiConfig>[] {
  return [getApi('minimal'), getApi('full')]
}

export function getApi(scope: ConfigType): NamedConfig<UserApi.ApiConfig> {
  const minimal: MinimalConfig<UserApi.ApiConfig> = {
    fn: getExtImport(scope, 'named'),
    httpRoute: getHttpRoute(scope),
  }

  return {
    name: scope === 'minimal' ? 'MinimalApi' : 'FullApi',
    config:
      scope === 'minimal'
        ? minimal
        : ({
            ...minimal,
            entities: [getEntity('task')],
            auth: true,
            middlewareConfigFn: getExtImport(scope, 'named'),
          } satisfies FullConfig<UserApi.ApiConfig>),
  }
}

export function getSchedule(
  scope: ConfigType
): MinimalConfig<UserApi.ScheduleConfig> | FullConfig<UserApi.ScheduleConfig> {
  const minimal: MinimalConfig<UserApi.ScheduleConfig> = {
    cron: '0 0 * * *',
  }

  return scope === 'minimal'
    ? minimal
    : ({
        ...minimal,
        args: { foo: 'bar' },
        executorOptions: {
          pgBoss: { jobOptions: { attempts: 3 } },
        },
      } satisfies FullConfig<UserApi.ScheduleConfig>)
}

export function getPerform(
  scope: ConfigType
): MinimalConfig<UserApi.Perform> | FullConfig<UserApi.Perform> {
  const minimal: MinimalConfig<UserApi.Perform> = {
    fn: getExtImport('minimal', 'named'),
  }

  return scope === 'minimal'
    ? minimal
    : ({
        ...minimal,
        executorOptions: {
          pgBoss: { jobOptions: { attempts: 3 } },
        },
      } satisfies FullConfig<UserApi.Perform>)
}

export function getJobs(): NamedConfig<UserApi.JobConfig>[] {
  return [getJob('minimal'), getJob('full')]
}

export function getJob(scope: ConfigType): NamedConfig<UserApi.JobConfig> {
  const minimal: MinimalConfig<UserApi.JobConfig> = {
    executor: 'PgBoss',
    perform: getPerform(scope),
  }

  return {
    name: scope === 'minimal' ? 'MinimalJob' : 'FullJob',
    config:
      scope === 'minimal'
        ? minimal
        : ({
            ...minimal,
            entities: [getEntity('task')],
            schedule: getSchedule(scope),
          } satisfies FullConfig<UserApi.JobConfig>),
  }
}
