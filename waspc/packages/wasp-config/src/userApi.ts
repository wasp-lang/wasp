/** This module defines the user-facing API for defining a Wasp app.
 */
import * as AppSpec from './appSpec.js'
import { GET_USER_SPEC } from './_private.js'

export class App {
  #userSpec: UserSpec;

  // NOTE: Using a non-public symbol gives us a pacakge-private property.
  [GET_USER_SPEC]() {
    return this.#userSpec
  }

  constructor(name: string, config: AppConfig) {
    this.#userSpec = {
      app: { name, config: config },
      actions: new Map(),
      apiNamespaces: new Map(),
      apis: new Map(),
      auth: undefined,
      client: undefined,
      cruds: new Map(),
      db: undefined,
      emailSender: undefined,
      jobs: new Map(),
      pages: new Map(),
      queries: new Map(),
      routes: new Map(),
      server: undefined,
      websocket: undefined,
    }
  }

  // TODO: Enforce that all methods are covered in compile time
  action(this: App, name: string, config: ActionConfig): void {
    this.#userSpec.actions.set(name, config)
  }

  apiNamespace(this: App, name: string, config: ApiNamespaceConfig): void {
    this.#userSpec.apiNamespaces.set(name, config)
  }

  api(this: App, name: string, config: ApiConfig): void {
    this.#userSpec.apis.set(name, config)
  }

  auth(this: App, config: AuthConfig): void {
    this.#userSpec.auth = config
  }

  client(this: App, config: ClientConfig): void {
    this.#userSpec.client = config
  }

  crud(this: App, name: string, config: Crud): void {
    this.#userSpec.cruds.set(name, config)
  }

  db(this: App, config: DbConfig): void {
    this.#userSpec.db = config
  }

  emailSender(this: App, config: EmailSenderConfig): void {
    this.#userSpec.emailSender = config
  }

  job(this: App, name: string, config: JobConfig): void {
    this.#userSpec.jobs.set(name, config)
  }

  page(this: App, name: string, config: PageConfig): PageName {
    this.#userSpec.pages.set(name, config)
    return name as PageName
  }

  query(this: App, name: string, config: QueryConfig): void {
    this.#userSpec.queries.set(name, config)
  }

  route(this: App, name: string, config: RouteConfig): void {
    this.#userSpec.routes.set(name, config)
  }

  server(this: App, config: ServerConfig): void {
    this.#userSpec.server = config
  }

  webSocket(this: App, config: WebsocketConfig) {
    this.#userSpec.websocket = config
  }
}

export type WaspConfig = AppSpec.Wasp

export type AppConfig = Pick<AppSpec.App, 'title' | 'wasp' | 'head'>

export type ExtImport = {
  import: string
  from: AppSpec.ExtImport['path']
} | {
  importDefault: string
  from: AppSpec.ExtImport['path']
}

export type ServerConfig = {
  setupFn: ExtImport
  middlewareConfigFn: ExtImport
}

export type PageConfig = {
  component: ExtImport
  authRequired?: boolean
}

export type WebsocketConfig = {
  fn: ExtImport
  autoConnect?: boolean
}

export type ClientConfig = {
  rootComponent: ExtImport
  setupFn: ExtImport
}

export type DbConfig = {
  seeds?: ExtImport[]
}

export type RouteConfig = {
  path: string
  to: PageName
}

type PageName = string & { _brand: 'Page' }

export type ActionConfig = {
  fn: ExtImport
  entities?: string[]
  auth?: boolean
}

export type ApiNamespaceConfig = {
  middlewareConfigFn: ExtImport
  path: string
}

export type ApiConfig = {
  fn: ExtImport
  middlewareConfigFn?: ExtImport
  entities?: string[]
  httpRoute: AppSpec.HttpRoute
  auth?: boolean
}

export type JobConfig = {
  executor: AppSpec.JobExecutor
  perform: Perform
  schedule?: ScheduleConfig
  entities?: string[]
}

export type Crud = {
  entity: string
  operations: CrudOperations
}

export type CrudOperations = {
  get?: CrudOperationOptions
  getAll?: CrudOperationOptions
  create?: CrudOperationOptions
  update?: CrudOperationOptions
  delete?: CrudOperationOptions
}

export type CrudOperationOptions = {
  isPublic?: boolean
  overrideFn?: ExtImport
}

export type Perform = {
  fn: ExtImport
  executorOptions?: ExecutorOptions
}

export type ScheduleConfig = {
  cron: string
  args?: object
  executorOptions?: ExecutorOptions
}

type ExecutorOptions = {
  // TODO: Type this better and test it
  // rewriting waspc/todoApp should make sure it works
  pgBoss: object
}

export type QueryConfig = {
  fn: ExtImport
  entities?: string[]
  auth?: boolean
}

export type EmailSenderConfig = AppSpec.EmailSender

export type AuthConfig = {
  userEntity: string
  methods: AuthMethods
  externalAuthEntity?: string
  onAuthFailedRedirectTo: string
  onAuthSucceededRedirectTo?: string
  onBeforeSignup?: ExtImport
  onAfterSignup?: ExtImport
  onBeforeOAuthRedirect?: ExtImport
  onBeforeLogin?: ExtImport
  onAfterLogin?: ExtImport
}

export type AuthMethods = {
  usernameAndPassword?: UsernameAndPasswordConfig
  discord?: ExternalAuthConfig
  google?: ExternalAuthConfig
  gitHub?: ExternalAuthConfig
  keycloak?: ExternalAuthConfig
  email?: EmailAuthConfig
}

export type UsernameAndPasswordConfig = {
  userSignupFields?: ExtImport
}

export type ExternalAuthConfig = {
  configFn?: ExtImport
  userSignupFields?: ExtImport
}

export type EmailAuthConfig = {
  userSignupFields?: ExtImport
  fromField: EmailFromField
  emailVerification: EmailVerificationConfig
  passwordReset: PasswordResetConfig
}

export type EmailSender = {
  provider: EmailProvider
  defaultFrom?: EmailFromField
}

export type EmailProvider = AppSpec.EmailProvider

export type EmailFromField = {
  name?: string
  email: string
}

export type EmailVerificationConfig = {
  getEmailContentFn?: ExtImport
  clientRoute: string
}

export type PasswordResetConfig = {
  getEmailContentFn?: ExtImport
  clientRoute: string
}

export type UserSpec = {
  app: { name: string; config: AppConfig }
  actions: Map<string, ActionConfig>
  apiNamespaces: Map<string, ApiNamespaceConfig>
  apis: Map<string, ApiConfig>
  auth?: AuthConfig
  client?: ClientConfig
  cruds: Map<string, Crud>
  db?: DbConfig
  emailSender?: EmailSenderConfig
  jobs: Map<string, JobConfig>
  pages: Map<string, PageConfig>
  queries: Map<string, QueryConfig>
  routes: Map<string, RouteConfig>
  server?: ServerConfig
  websocket?: WebsocketConfig
}
